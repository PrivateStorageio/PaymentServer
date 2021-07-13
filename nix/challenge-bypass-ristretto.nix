# Provide the ffi bindings to the Rust challenge-bypass-ristretto library.
{ fetchFromGitHub, callPackage }:
let
  src = import ./challenge-bypass-ristretto-repo.nix;
in
  import "${src}/default-challenge-bypass-ristretto-ffi.nix" { }
