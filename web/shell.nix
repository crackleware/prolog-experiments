{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    nodePackages.live-server
  ];

  live_serve = ''
    live-server --host=127.0.0.1 --port=8080 --open=app.html --watch=app.html,app.js,app.pl
  '';

  shellHook = ''
  '';
}
