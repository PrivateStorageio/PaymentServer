# shell.nix
{ pkgs ? import <nixpkgs> { } }:
let
  project = import ./nix/default.nix;
in
  project.shellFor {
    # Prevents cabal from choosing alternate plans, so that
    # *all* dependencies are provided by Nix.
    exactDeps = true;

    withHoogle = false;

    buildInputs = [
      project.pkgs.stack
      project.pkgs.haskell-language-server
    ];
  }
