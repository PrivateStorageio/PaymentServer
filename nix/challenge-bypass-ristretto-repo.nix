let
  pkgs = import <nixpkgs> {};
in
  pkgs.fetchFromGitHub {
    owner = "LeastAuthority";
    repo = "python-challenge-bypass-ristretto";
    rev = "v2020.04.03rc1";
    sha256 = "03iqww9h7ff0pdi3mgg912a9s9yd8xbm4cx7iznv94611vxrcdzs";
  }
