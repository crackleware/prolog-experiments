git clone --recurse-submodules ~/work/my/prolog-experiments
cd prolog-experiments/web
nix develop -c live-server --host=127.0.01 --port=8080 --open=app.html --watch='app.html,app.js,app.pl'

Browser should be already showing page at http://127.0.0.1:8080/app.html
