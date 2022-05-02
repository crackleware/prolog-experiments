Live demo of TodoList and Counter apps: https://crackleware.github.io/prolog-experiments/web/app.html

This is small single-page web application library currently implemented using http://tau-prolog.org inspired by https://elm-lang.org (view, update).

Before experimenting with advanced functionality like:

- automatically supported voice control interface

- machine understanding of arbitrary web page by auto combining components from Prolog specified component library

We need to speed-up execution of Prolog engine.

To develop:

git clone --recurse-submodules ~/work/my/prolog-experiments
cd prolog-experiments/web
nix develop -c sh -c '$live_serve'

Browser should open page at http://127.0.0.1:8080/app.html
