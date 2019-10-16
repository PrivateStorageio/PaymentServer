{ fetchFromGitHub, callPackage }:
let
  src = fetchFromGitHub {
    owner = "LeastAuthority";
    repo = "privacypass";
    rev = "45855401e163f8e622bd93a5c5bce13de8c8510a";
    sha256 = "15wv8vas6x8cdicylp0m632c916p7qxq1k4lnchr8c92lldp0rv7";
  };
in
  callPackage "${src}/ristretto.nix" { }
