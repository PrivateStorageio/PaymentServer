let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).

      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).

      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).

      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).

      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).

      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.

      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  {
    flags = {};
    package = {
      specVersion = "1.10";
      identifier = { name = "PaymentServer"; version = "0.1.0.0"; };
      license = "Apache-2.0";
      copyright = "2019 Private Storage.io, LLC.";
      maintainer = "support@privatestorage.io";
      author = "Jean-Paul Calderone";
      homepage = "https://github.com/privatestorageio/PaymentServer#readme";
      url = "";
      synopsis = "Coordinate entities for the purchase of PrivateStorage.io vouchers.";
      description = "";
      buildType = "Simple";
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."base" or (buildDepError "base"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."utf8-string" or (buildDepError "utf8-string"))
          (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
          (hsPkgs."aeson" or (buildDepError "aeson"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
          (hsPkgs."wai-cors" or (buildDepError "wai-cors"))
          (hsPkgs."data-default" or (buildDepError "data-default"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."warp-tls" or (buildDepError "warp-tls"))
          (hsPkgs."stripe-core" or (buildDepError "stripe-core"))
          (hsPkgs."stripe-haskell" or (buildDepError "stripe-haskell"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."containers" or (buildDepError "containers"))
          (hsPkgs."cryptonite" or (buildDepError "cryptonite"))
          (hsPkgs."sqlite-simple" or (buildDepError "sqlite-simple"))
          (hsPkgs."retry" or (buildDepError "retry"))
          (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
          (hsPkgs."servant-prometheus" or (buildDepError "servant-prometheus"))
          ];
        pkgconfig = [
          (pkgconfPkgs."libchallenge_bypass_ristretto_ffi" or (pkgConfDepError "libchallenge_bypass_ristretto_ffi"))
          ];
        };
      exes = {
        "PaymentServer-exe" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."PaymentServer" or (buildDepError "PaymentServer"))
            ];
          };
        "PaymentServer-generate-key" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."PaymentServer" or (buildDepError "PaymentServer"))
            ];
          };
        };
      tests = {
        "PaymentServer-tests" = {
          depends = [
            (hsPkgs."base" or (buildDepError "base"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."text" or (buildDepError "text"))
            (hsPkgs."tasty" or (buildDepError "tasty"))
            (hsPkgs."tasty-hunit" or (buildDepError "tasty-hunit"))
            (hsPkgs."directory" or (buildDepError "directory"))
            (hsPkgs."async" or (buildDepError "async"))
            (hsPkgs."sqlite-simple" or (buildDepError "sqlite-simple"))
            (hsPkgs."http-types" or (buildDepError "http-types"))
            (hsPkgs."wai" or (buildDepError "wai"))
            (hsPkgs."wai-extra" or (buildDepError "wai-extra"))
            (hsPkgs."servant-server" or (buildDepError "servant-server"))
            (hsPkgs."prometheus-client" or (buildDepError "prometheus-client"))
            (hsPkgs."stripe-core" or (buildDepError "stripe-core"))
            (hsPkgs."PaymentServer" or (buildDepError "PaymentServer"))
            ];
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ../.; }
