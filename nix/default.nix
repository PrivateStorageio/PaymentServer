{ pkgs ? import <nixpkgs> { } }:

let
  # Get our overlay in place regardless of whether a value is passed for pkgs.
  # The build fails without it and it's unreasonable to expect our caller to
  # know to apply it.
  nixpkgs = import pkgs.path { overlays = [ (import ./overlay.nix) ]; };

  # Pin a particular version of haskell.nix.  The particular version isn't
  # special.  It's just recent at the time this expression was written and it
  # is known to work with PaymentServer.  It could be bumped if necessary but
  # this would probably only happen as a result of bumping the resolver in
  # stack.yaml.
  haskell = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/0cb32e695d7014908fb01fd7e3d225ea33dbdc98.tar.gz) { pkgs = nixpkgs; };

  pkgSet = haskell.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
