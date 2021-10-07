{ system
  , compiler
  , flags
  , pkgs
  , hsPkgs
  , pkgconfPkgs
  , errorHandler
  , config
  , ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "PaymentServer"; version = "0.1.1.1"; };
      license = "Apache-2.0";
      copyright = "2019 Private Storage.io, LLC.";
      maintainer = "support@privatestorage.io";
      author = "Jean-Paul Calderone";
      homepage = "https://github.com/privatestorageio/PaymentServer#readme";
      url = "";
      synopsis = "Coordinate entities for the purchase of PrivateStorage.io vouchers.";
      description = "";
      buildType = "Simple";
      isLocal = true;
      detailLevel = "FullDetails";
      licenseFiles = [ "LICENSE" ];
      dataDir = ".";
      dataFiles = [];
      extraSrcFiles = [ "README.rst" ];
      extraTmpFiles = [];
      extraDocFiles = [];
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (errorHandler.buildDepError "base"))
          (hsPkgs."optparse-applicative" or (errorHandler.buildDepError "optparse-applicative"))
          (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
          (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
          (hsPkgs."utf8-string" or (errorHandler.buildDepError "utf8-string"))
          (hsPkgs."servant" or (errorHandler.buildDepError "servant"))
          (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
          (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
          (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
          (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
          (hsPkgs."wai-cors" or (errorHandler.buildDepError "wai-cors"))
          (hsPkgs."data-default" or (errorHandler.buildDepError "data-default"))
          (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
          (hsPkgs."warp-tls" or (errorHandler.buildDepError "warp-tls"))
          (hsPkgs."stripe-haskell" or (errorHandler.buildDepError "stripe-haskell"))
          (hsPkgs."stripe-core" or (errorHandler.buildDepError "stripe-core"))
          (hsPkgs."text" or (errorHandler.buildDepError "text"))
          (hsPkgs."containers" or (errorHandler.buildDepError "containers"))
          (hsPkgs."cryptonite" or (errorHandler.buildDepError "cryptonite"))
          (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
          (hsPkgs."retry" or (errorHandler.buildDepError "retry"))
          (hsPkgs."prometheus-client" or (errorHandler.buildDepError "prometheus-client"))
          (hsPkgs."servant-prometheus" or (errorHandler.buildDepError "servant-prometheus"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libchallenge_bypass_ristretto_ffi" or (errorHandler.pkgConfDepError "libchallenge_bypass_ristretto_ffi"))
          ];
        buildable = true;
        modules = [
          "PaymentServer/Processors/Stripe"
          "PaymentServer/Ristretto"
          "PaymentServer/Issuer"
          "PaymentServer/Persistence"
          "PaymentServer/Redemption"
          "PaymentServer/Metrics"
          "PaymentServer/Server"
          "PaymentServer/Main"
          ];
        hsSourceDirs = [ "src" ];
        };
      exes = {
        "PaymentServer-exe" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."PaymentServer" or (errorHandler.buildDepError "PaymentServer"))
            ];
          buildable = true;
          hsSourceDirs = [ "app" ];
          mainPath = [ "Main.hs" ];
          };
        "PaymentServer-generate-key" = {
          depends = [
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."PaymentServer" or (errorHandler.buildDepError "PaymentServer"))
            ];
          buildable = true;
          hsSourceDirs = [ "generate-key" ];
          mainPath = [ "Main.hs" ];
          };
        };
      tests = {
        "PaymentServer-tests" = {
          depends = [
            (hsPkgs."aeson" or (errorHandler.buildDepError "aeson"))
            (hsPkgs."base" or (errorHandler.buildDepError "base"))
            (hsPkgs."bytestring" or (errorHandler.buildDepError "bytestring"))
            (hsPkgs."text" or (errorHandler.buildDepError "text"))
            (hsPkgs."transformers" or (errorHandler.buildDepError "transformers"))
            (hsPkgs."raw-strings-qq" or (errorHandler.buildDepError "raw-strings-qq"))
            (hsPkgs."time" or (errorHandler.buildDepError "time"))
            (hsPkgs."tasty" or (errorHandler.buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (errorHandler.buildDepError "tasty-hunit"))
            (hsPkgs."directory" or (errorHandler.buildDepError "directory"))
            (hsPkgs."async" or (errorHandler.buildDepError "async"))
            (hsPkgs."sqlite-simple" or (errorHandler.buildDepError "sqlite-simple"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."wai" or (errorHandler.buildDepError "wai"))
            (hsPkgs."wai-extra" or (errorHandler.buildDepError "wai-extra"))
            (hsPkgs."warp" or (errorHandler.buildDepError "warp"))
            (hsPkgs."http-types" or (errorHandler.buildDepError "http-types"))
            (hsPkgs."servant-server" or (errorHandler.buildDepError "servant-server"))
            (hsPkgs."prometheus-client" or (errorHandler.buildDepError "prometheus-client"))
            (hsPkgs."stripe-core" or (errorHandler.buildDepError "stripe-core"))
            (hsPkgs."PaymentServer" or (errorHandler.buildDepError "PaymentServer"))
            ];
          buildable = true;
          modules = [
            "Persistence"
            "Redemption"
            "Metrics"
            "Stripe"
            "FakeStripe"
            ];
          hsSourceDirs = [ "test" ];
          mainPath = [ "Spec.hs" ];
          };
        };
      };
    } // rec {
    src = (pkgs.lib).mkDefault ./.;
    }