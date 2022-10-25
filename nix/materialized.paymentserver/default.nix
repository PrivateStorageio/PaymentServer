{
  extras = hackage:
    {
      packages = {
        "network" = (((hackage.network)."3.1.2.7").revisions).default;
        "stripe-signature" = (((hackage.stripe-signature)."1.0.0.14").revisions).default;
        PaymentServer = ./PaymentServer.nix;
        servant-prometheus = ./.stack-to-nix.cache.0;
        };
      };
  resolver = "lts-18.28";
  modules = [
    ({ lib, ... }:
      { packages = {}; })
    { packages = {}; }
    ({ lib, ... }:
      { planned = lib.mkOverride 900 true; })
    ];
  }