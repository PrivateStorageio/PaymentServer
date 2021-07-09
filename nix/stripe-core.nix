{ mkDerivation, aeson, base, bytestring, fetchgit, lib, mtl, text
, time, transformers, unordered-containers
}:
mkDerivation {
  pname = "stripe-core";
  version = "2.5.0";
  src = fetchgit {
    url = "https://github.com/dmjio/stripe.git";
    sha256 = "0hwc0x7vs25047rybnm71x4c5v37k69iq66g697hdal0ccnmadns";
    rev = "913b88e7c71c783549919a6019ad5cd9306f80e8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/stripe-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    aeson base bytestring mtl text time transformers
    unordered-containers
  ];
  homepage = "https://github.com/dmjio/stripe-haskell";
  description = "Stripe API for Haskell - Pure Core";
  license = lib.licenses.mit;
}
