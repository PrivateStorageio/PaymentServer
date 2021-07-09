{ mkDerivation, aeson, base, bytestring, fetchgit, free, hspec
, hspec-core, lib, mtl, random, stripe-core, text, time
, transformers, unordered-containers
}:
mkDerivation {
  pname = "stripe-tests";
  version = "2.5.0";
  src = fetchgit {
    url = "https://github.com/dmjio/stripe.git";
    sha256 = "0hwc0x7vs25047rybnm71x4c5v37k69iq66g697hdal0ccnmadns";
    rev = "913b88e7c71c783549919a6019ad5cd9306f80e8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/stripe-tests; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring free hspec hspec-core mtl random stripe-core
    text time transformers unordered-containers
  ];
  homepage = "https://github.com/dmjio/stripe-haskell";
  description = "Tests for Stripe API bindings for Haskell";
  license = lib.licenses.mit;
}
