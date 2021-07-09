{ mkDerivation, ansi-terminal, array, base, call-stack, clock
, deepseq, directory, fetchgit, filepath, hpack, hspec-expectations
, hspec-meta, HUnit, lib, process, QuickCheck, quickcheck-io
, random, setenv, silently, stm, temporary, tf-random, transformers
}:
mkDerivation {
  pname = "hspec-core";
  version = "2.6.1";
  src = fetchgit {
    url = "https://github.com/hspec/hspec";
    sha256 = "0nqvkp6a8ii4sgv9cl4vr0f2bby4a91m6sr5sjlv0nvycqwin69m";
    rev = "95ad518ae49ef6596e7ed55c47084cd75045c169";
    fetchSubmodules = true;
  };
  postUnpack = "sourceRoot+=/hspec-core; echo source root reset to $sourceRoot";
  libraryHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations HUnit QuickCheck quickcheck-io random
    setenv stm tf-random transformers
  ];
  libraryToolDepends = [ hpack ];
  testHaskellDepends = [
    ansi-terminal array base call-stack clock deepseq directory
    filepath hspec-expectations hspec-meta HUnit process QuickCheck
    quickcheck-io random setenv silently stm temporary tf-random
    transformers
  ];
  testToolDepends = [ hspec-meta ];
  testTarget = "--test-option=--skip --test-option='Test.Hspec.Core.Runner.hspecResult runs specs in parallel'";prePatch = "hpack";
  homepage = "http://hspec.github.io/";
  description = "A Testing Framework for Haskell";
  license = lib.licenses.mit;
}
