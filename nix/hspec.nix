{ mkDerivation, base, fetchgit, hpack, hspec-core, hspec-discover
, hspec-expectations, lib, QuickCheck
}:
mkDerivation {
  pname = "hspec";
  version = "2.6.1";
  src = fetchgit {
    url = "https://github.com/hspec/hspec";
    sha256 = "0nqvkp6a8ii4sgv9cl4vr0f2bby4a91m6sr5sjlv0nvycqwin69m";
    rev = "95ad518ae49ef6596e7ed55c47084cd75045c169";
    fetchSubmodules = true;
  };
  libraryHaskellDepends = [
    base hspec-core hspec-discover hspec-expectations QuickCheck
  ];
  libraryToolDepends = [ hpack ];
  prePatch = "hpack";
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = lib.licenses.mit;
}
