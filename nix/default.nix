{ pkgs ? import <nixpkgs> { overlays = [ (import ./overlay.nix) ]; } }:

let
  # Pin a particular version of haskell.nix.  The particular version isn't
  # special.  It's just recent at the time this expression was written and it
  # is known to work with PaymentServer.  It could be bumped if necessary but
  # this would probably only happen as a result of bumping the resolver in
  # stack.yaml.
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/0cb32e695d7014908fb01fd7e3d225ea33dbdc98.tar.gz) { inherit pkgs; };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
