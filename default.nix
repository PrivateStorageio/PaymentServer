let
  # Read in the Niv sources
  sources = import ./nix/sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-2009
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;
    libchallenge_bypass_ristretto_ffi = pkgs.callPackage ./oldnix/challenge-bypass-ristretto.nix { };

in pkgs.haskell-nix.project {
  # 'cleanGit' cleans a source directory based on the files known by git
  src = pkgs.haskell-nix.haskellLib.cleanGit {
    name = "PaymentServer";
    src = ./.;
  };
  modules = [{
    packages.PaymentServer.components.library.pkgconfig = pkgs.lib.mkForce [
      [libchallenge_bypass_ristretto_ffi]
    ];
    packages.PaymentServer.components.library.libs = pkgs.lib.mkForce [
      libchallenge_bypass_ristretto_ffi
    ];
  }];
}