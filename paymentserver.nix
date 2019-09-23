{ mkDerivation, aeson, base, containers, cryptonite, data-default
, optparse-applicative, ristretto, servant, servant-server, stdenv
, stripe-core, text, wai, wai-extra, warp
}:
mkDerivation {
  pname = "PaymentServer";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson base containers cryptonite data-default optparse-applicative
    servant servant-server stripe-core text wai wai-extra warp
  ];
  libraryPkgconfigDepends = [ ristretto ];
  executableHaskellDepends = [ base text ];
  homepage = "https://github.com/privatestorageio/PaymentServer#readme";
  description = "Coordinate entities for the purchase of PrivateStorage.io vouchers.";
  license = stdenv.lib.licenses.asl20;
}
