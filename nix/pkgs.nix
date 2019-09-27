{
  extras = hackage:
    {
      packages = ({
        "stripe-core" = (((hackage.stripe-core)."2.5.0").revisions).default;
        } // { PaymentServer = ./PaymentServer.nix; }) // {};
      };
  resolver = "lts-14.1";
  }