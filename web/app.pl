%% imports

:- use_module(library(lists)).
:- use_module(library(dom)).
:- use_module(library(statistics)).
:- use_module(library(js)).
:- use_module(library(format)).
:- use_module(library(charsio)).

:- op(850, xfx, =>).

%% app

% init_state(State) :- update(show(counter), _, State).
init_state(State) :- update(show(todolist), _, State).

wrapper(CurrentPage, Doc) => div([
  style('
    body, input, button, div {
      background: black;
      color: orange;
      font-size: 1.1em;
      border: solid 1px black;
    }
    button:hover { border: solid 1px; }
  '),
  navbar(CurrentPage),
  Doc]).

view(navbar(CurrentPage), div(Btns)) :-
    findall(
      btn(Page, show(Page), [stylemap=[background=BgCol]]),
      ( % clause(update(show(Page), _, _), _), % doesn't work in tau-prolog?
        member(Page, [
          todolist,
          counter
        ]),
        (Page == CurrentPage -> BgCol = '#600'; BgCol = transparent)),
      Btns).

btn(Text, Action) => btn(Text, Action, []).
btn(Text, Action, Attrs) => button([click=action(Action)|Attrs], Text).

%%% counter page

update(show(counter), _, counter(0)).
update(increase, counter(State), counter(NewState)) :- NewState is State + 1.
update(decrease, counter(State), counter(NewState)) :- NewState is State - 1.

counter(State) => wrapper(counter, div([
  div(['click ', btn('+', increase), btn('-', decrease), ' to increase/decrease counter']),
  div(['count: ', State])
])).

%%% todolist page

update(show(todolist), _, todolist([
  todo('buy milk', active),
  todo('buy grass', active)])).

update(add_todo_item, input_entered(Title),
  todolist(Items), todolist([todo(Title, active)|Items])).

update(todo_remove_item(Title), todolist(Items), todolist(Items2)) :-
  exclude('='(todo(Title, _)), Items, Items2).

update(todo_toggle_status(Title), todolist(Items), todolist(Items2)) :-
  append(Before, [todo(Title, Status)|After], Items),
  (Status == active -> Status2 = done; Status2 = active),
  append(Before, [todo(Title, Status2)|After], Items2).

todolist(Items) => wrapper(todolist, div([
  div(maplist(todo_item, Items)),
  div([input([
    keyup=action(add_todo_item),
    placeholder='new item'], [])])
])).

todo_item(todo(Title, Status)) => div([
  btn('✔', todo_toggle_status(Title), [stylemap=[
    background=blue, color=if(Status=done, yellow, transparent)]]),
  Title, ' ',
  btn('⨯', todo_remove_item(Title))
]).

%% framework

view(Var, Var) :- var(Var).

view(State, Doc) :- State => Doc.

view(maplist(Tag, List), Doc) :-
  E =.. [Tag, X],
  findall(E, member(X, List), Doc).

view(if(Cond, True, False), Result) :-
  Cond -> Result = True; Result = False.

view(stylemap=List, style=S) :-
  view(List, List2),
  stylemap(List2, S).

view(Doc1, Doc2) :-
  Doc1 =.. [F|Args],
  maplist(view, Args, Args2),
  Doc2 =.. [F|Args2].

view(Doc, Doc).

view_rec(Doc1, Doc2) :-
  view(Doc1, D), !,
  % log([view_rec, D]),
  (
    Doc1 \= D, !, view_rec(D, Doc2)
  ;
    Doc2 = D
  ).

stylemap(Map, CSS) :-
  findall(S, (
    member(Property=Value, Map),
    atomic_list_concat([Property, ': ', Value], S)),
    Strs),
  atomic_list_concat(Strs, '; ', CSS).

render :-
  state(State),
  view_rec(State, Doc), !,
  % log([render, Doc]),
  doc_html(Doc, Element),
  dom:get_by_id(out, OutElement),
  dom:set_html(OutElement, ''),
  dom:append_child(OutElement, Element).

doc_html(Doc, HTMLElement) :-
  Doc =.. [Functor|Args], !,
  doc_html(Functor, Args, HTMLElement).

doc_html(Functor, Args, HTMLElement) :-
  dom:create(Functor, HTMLElement),
  (
    [Attrs, Children] = Args,
    forall(member(Attr=Value, Attrs),
                  doc_element_set_attr(HTMLElement, Attr=Value))
  ;
    [Children] = Args
  ),
  doc_html_children(Children, HTMLElement).

doc_text(Doc, Text) :-
  (
    atom(Doc), !, Text = Doc
  ;
    number(Doc), term_text(Doc, Text)
  ).

doc_html_children(Children, HTMLElement) :- is_list(Children), !,
  forall(member(ChildDoc, Children), (
    (
      doc_text(ChildDoc, Text), !,
      document(Document), %% can't use dom:document here??
      apply(Document, createTextNode, [Text], ChildElement), %% can't use js:apply here??
      apply(HTMLElement, appendChild, [ChildElement], _)
    ;
      doc_html(ChildDoc, ChildElement),
      append_child(HTMLElement, ChildElement)
    ))).

doc_html_children(Children, HTMLElement) :-
  % term_text(Children, Text),
  doc_text(Children, Text), !,
  js:set_prop(HTMLElement, innerText, Text).

doc_element_set_attr(HTMLElement, Attr=action(Action)) :- !,
  dom:bind(HTMLElement, Attr, Evt, update_state(Attr, Evt, Action)).

doc_element_set_attr(HTMLElement, Attr=Value) :-
  dom:set_attr(HTMLElement, Attr, Value).

update_state(Attr, Evt, Action) :-
  state(State),
  (
    Attr == keyup,
    get_prop(Evt, key, Key), Key = 'Enter',
    get_prop(Evt, target, Target),
    get_prop(Target, tagName, 'INPUT'),
    get_prop(Target, value, Value),
    update(Action, input_entered(Value), State, NewState),
    !
  ;
    update(Action, State, NewState)
  ),
  log(['update_state', Attr, Evt, Action, State, NewState]),
  retractall(state(_)),
  asserta(state(NewState)),
  render.

%% utils

log(X) :- writeq(X).

jseval(Code) :-
  jseval(Code, _).
jseval(Code, Result) :-
  js:apply(eval, [Code], Result).

term_text(Term, Text) :-
  charsio:writeq_to_chars(Term, Chars),
  atom_chars(Text, Chars).

%% tests

tests :-
  forall(member(Test, [
      stylemap(
        [abc=123, ddd='bold blue'],
        'abc: 123; ddd: bold blue'),
      true
    ]), (
    ( Test -> Status = ok; Status = failed ),
    ( Status == failed -> log([test,Status,Test]); true))).

%% main

run1  :-
  init_state(State),
  view_rec(State, Doc),
  log(Doc),
  true.

run :-
  tests,
  init_state(State),
  retractall(state(_)),
  asserta(state(State)),
  render.
