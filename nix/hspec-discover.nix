{ mkDerivation, base, directory, fetchgit, filepath, hpack
, hspec-meta, lib, QuickCheck
}:
mkDerivation {
  pname = "hspec-discover";
  version = "2.6.1";
  src = fetchgit {
    url = "https://github.com/hspec/hspec";
    sha256 = "0nqvkp6a8ii4sgv9cl4vr0f2bby4a91m6sr5sjlv0nvycqwin69m";
    rev = "95ad518ae49ef6596e7ed55c47084cd75045c169";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/hspec-discover; echo source root reset to $sourceRoot";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base directory filepath ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base directory filepath ];
  testHaskellDepends = [
    base directory filepath hspec-meta QuickCheck
  ];
  testToolDepends = [ hspec-meta ];
  prePatch = "hpack";
  homepage = "http://hspec.github.io/";
  description = "Automatically discover and run Hspec tests";
  license = lib.licenses.mit;
}
