{ mkDerivation, aeson, base, bytestring, fetchgit, free, hspec
, http-client, http-client-tls, http-types, lib, stripe-core
, stripe-tests, text
}:
mkDerivation {
  pname = "stripe-http-client";
  version = "2.5.0";
  src = fetchgit {
    url = "https://github.com/dmjio/stripe.git";
    sha256 = "0hwc0x7vs25047rybnm71x4c5v37k69iq66g697hdal0ccnmadns";
    rev = "913b88e7c71c783549919a6019ad5cd9306f80e8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/stripe-http-client; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring http-client http-client-tls http-types
    stripe-core text
  ];
  testHaskellDepends = [
    base free hspec http-client stripe-core stripe-tests
  ];
  description = "Stripe API for Haskell - http-client backend";
  license = lib.licenses.mit;
}
