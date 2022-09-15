{
  extras = hackage:
    {
      packages = {
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