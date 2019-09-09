let
  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/3c83ad6ac13b67101cc3e2e07781963a010c1624.tar.gz";
    sha256 = "0cdq342wrkvkyccygpp1gvwp7hhqg68hljjwld4vjixm901ayy14";
  }) {};
in
{ pkgs ? pkgs }:
pkgs.mkShell {
  buildInputs = [
    pkgs.stack
  ];
}
