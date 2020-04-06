let
  # Pin a particular version of haskell.nix.  The particular version isn't
  # special.  It's just recent at the time this expression was written and it
  # is known to work with PaymentServer.  It could be bumped if necessary but
  # this would probably only happen as a result of bumping the resolver in
  # stack.yaml.
  haskellNix = import (builtins.fetchTarball https://github.com/input-output-hk/haskell.nix/archive/d57402225e125369805fb262f3d7497dabb8dc82.tar.gz) {};
  nixpkgsSrc = haskellNix.sources.nixpkgs-1909;
  nixpkgsArgs = haskellNix.nixpkgsArgs;
in
{ pkgs ? import nixpkgsSrc (nixpkgsArgs // { overlays = nixpkgsArgs.overlays ++ [ (import ./overlay.nix) ]; })
}:
pkgs.haskell-nix.stackProject {
  src = pkgs.haskell-nix.haskellLib.cleanGit { name = "PaymentServer"; src = ../.; };
}
