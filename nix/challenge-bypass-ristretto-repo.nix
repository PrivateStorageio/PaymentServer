let
  pkgs = import <nixpkgs> {};
in
  pkgs.fetchFromGitHub {
    owner = "LeastAuthority";
    repo = "python-challenge-bypass-ristretto";
    rev = "v2021.07.12";
    sha256 = "16af1qmx7srhvcc936x7hl2bz50hafm39311dbzqam9ms1i5q89j";
  }
