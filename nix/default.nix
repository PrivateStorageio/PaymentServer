let
  moreOverlays = [ (import ./overlay.nix) ];

  # Read in the Niv sources
  sources = import ./sources.nix { };
  # If ./sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix { };
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Haskell.nix is delivery as an overlay.  Add our own overlay, which
  # provides one of our crypto dependencies, in a non-destructive way.
  allOverlays = moreOverlays ++ haskellNix.nixpkgsArgs.overlays;

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  pkgs = import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-2205
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    (haskellNix.nixpkgsArgs // { overlays = allOverlays; });
in
  pkgs.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "PaymentServer";
      src = ../.;
    };

    # Cause the expressions at this path to be used, rather than dynamically
    # generating them all from other sources.  This is a great memory savings
    # (some half GB or so of VmPeak shaved).  For instructions about ongoing
    # maintenance of these expressions, see
    # https://input-output-hk.github.io/haskell.nix/tutorials/materialization/
    materialized = ./materialized.paymentserver;
    # See materialized.paymentserver/README
    # checkMaterialization = true;
  }
