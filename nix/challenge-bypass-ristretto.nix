# Provide the ffi bindings to the Rust challenge-bypass-ristretto library.
let
  sources = import ./sources.nix;
in
{ fetchFromGitHub
, callPackage
, libchallenge_bypass_ristretto_ffi_repo ? sources.libchallenge_bypass_ristretto_ffi
}:
  import "${libchallenge_bypass_ristretto_ffi_repo}/challenge-bypass-ristretto.nix" { }
