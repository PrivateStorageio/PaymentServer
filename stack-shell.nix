# This is intended to be used as the shell-file for the stack configuration.
# It sets up the non-Haskell parts of the stack build environment.
{ ghc }:
let
  pkgs = import <nixpkgs> { };
  # Get our Ristretto bindings.
  libchallenge_bypass_ristretto = pkgs.callPackage ./nix/challenge-bypass-ristretto.nix { };
in
  # This is what you're supposed to call in a stack shell-file.  I don't
  # *really* know what it does but I know it works...
  pkgs.haskell.lib.buildStackProject {
    inherit ghc;
    name = "PrivacyPass";
    # zlib is a common dependency of many of our dependencies.  and we put our
    # ristretto library in as well.
    buildInputs = [ pkgs.zlib libchallenge_bypass_ristretto ];
  }
