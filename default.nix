{ pkgs ? import <nixpkgs> { } }:
with pkgs;
mkShell {
  buildInputs = [
    elmPackages.elm
    elmPackages.elm-test
    elmPackages.elm-format
  ];
}
