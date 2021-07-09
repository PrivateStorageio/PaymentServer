with import <nixpkgs> { };
haskell.packages.ghc865.callPackage ./PaymentServer.nix {
  libchallenge_bypass_ristretto_ffi = callPackage ./nix/challenge-bypass-ristretto.nix { };
  servant-prometheus = haskell.packages.ghc865.callPackage ./nix/servant-prometheus.nix { };
  stripe-core = haskell.packages.ghc865.callPackage ./nix/stripe-core.nix { };
  stripe-haskell = haskell.packages.ghc865.callPackage ./nix/stripe-haskell.nix {
    stripe-http-client = haskell.packages.ghc865.callPackage ./nix/stripe-http-client.nix {
      stripe-tests = haskell.packages.ghc865.callPackage ./nix/stripe-tests.nix rec {
        hspec = haskell.packages.ghc865.callPackage ./nix/hspec.nix {
          inherit hspec-core;
          hspec-discover = haskell.packages.ghc865.callPackage ./nix/hspec-discover.nix { };
        };
        hspec-core = haskell.packages.ghc865.callPackage ./nix/hspec-core.nix { };
      };
    };
  };
}
