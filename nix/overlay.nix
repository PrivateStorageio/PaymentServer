self: super: {
  # Put the library output of the bindings package into nixpkgs where the
  # Haskell/Nix build toolchain can find it.  Note we grab the lib output from
  # the derivation because that's where the nixpkgs Rust tooling seems to put
  # all of the good files nowadays.
  libchallenge_bypass_ristretto_ffi = (super.callPackage ./challenge-bypass-ristretto.nix { }).lib;

  # And map the name of the package in the pkg-config-resolution parts of
  # Haskell.nix so that when we ask for it by name from the cabal file it can
  # be resolved.
  haskell-nix = super.haskell-nix // {
    extraPkgconfigMappings = super.haskell-nix.extraPkgconfigMappings // {
      "libchallenge_bypass_ristretto_ffi" = [ "libchallenge_bypass_ristretto_ffi" ];
    };
  };
}
