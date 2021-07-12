self: super: {
  # Put the library output of the bindings package into nixpkgs where the
  # Haskell/Nix build toolchain can find it.  Note we grab the lib output from
  # the derivation because that's where the nixpkgs Rust tooling seems to put
  # all of the good files nowadays.
  libchallenge_bypass_ristretto_ffi = (super.callPackage ./challenge-bypass-ristretto.nix { }).lib;
}
