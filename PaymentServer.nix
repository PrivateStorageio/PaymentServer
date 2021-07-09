{ mkDerivation, aeson, async, base, bytestring, containers
, cryptonite, data-default, directory, http-types, lib
, libchallenge_bypass_ristretto_ffi, optparse-applicative
, prometheus-client, raw-strings-qq, retry, servant
, servant-prometheus, servant-server, sqlite-simple, stripe-core
, stripe-haskell, tasty, tasty-hunit, text, time, transformers
, utf8-string, wai, wai-cors, wai-extra, warp, warp-tls
}:
mkDerivation {
  pname = "PaymentServer";
  version = "0.1.1.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base bytestring containers cryptonite data-default http-types
    optparse-applicative prometheus-client retry servant
    servant-prometheus servant-server sqlite-simple stripe-core
    stripe-haskell text utf8-string wai wai-cors wai-extra warp
    warp-tls
  ];
  libraryPkgconfigDepends = [ libchallenge_bypass_ristretto_ffi ];
  executableHaskellDepends = [ base text ];
  testHaskellDepends = [
    async base bytestring directory http-types prometheus-client
    raw-strings-qq servant-server sqlite-simple stripe-core tasty
    tasty-hunit text time transformers wai wai-extra warp
  ];
  homepage = "https://github.com/privatestorageio/PaymentServer#readme";
  description = "Coordinate entities for the purchase of PrivateStorage.io vouchers.";
  license = lib.licenses.asl20;
}
