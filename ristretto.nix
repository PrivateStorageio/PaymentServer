{ fetchFromGitHub, callPackage }:
let
  src = fetchFromGitHub {
    owner = "LeastAuthority";
    repo = "privacypass";
    rev = "f74b371cdf179454f3ad540a4d0deea879fbe5e1";
    sha256 = "0a020ks8awlpil58zcaj5apk1ls0q2y492wsh62kl529jp518v4b";
  };
in
  callPackage "${src}/ristretto.nix" { }
