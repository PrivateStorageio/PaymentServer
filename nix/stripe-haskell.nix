{ mkDerivation, base, fetchgit, lib, stripe-core
, stripe-http-client
}:
mkDerivation {
  pname = "stripe-haskell";
  version = "2.5.0";
  src = fetchgit {
    url = "https://github.com/dmjio/stripe.git";
    sha256 = "0hwc0x7vs25047rybnm71x4c5v37k69iq66g697hdal0ccnmadns";
    rev = "913b88e7c71c783549919a6019ad5cd9306f80e8";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/stripe-haskell; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [ base stripe-core stripe-http-client ];
  homepage = "https://github.com/dmjio/stripe";
  description = "Stripe API for Haskell";
  license = lib.licenses.mit;
}
