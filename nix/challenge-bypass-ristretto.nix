{ fetchFromGitHub, callPackage }:
let
  src = import ./challenge-bypass-ristretto-repo.nix;
in
  callPackage "${src}/challenge-bypass-ristretto.nix" { }
