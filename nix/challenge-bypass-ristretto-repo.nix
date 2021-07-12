let
  pkgs = import <nixpkgs> {};
in
  pkgs.fetchFromGitHub {
    owner = "LeastAuthority";
    repo = "python-challenge-bypass-ristretto";
    rev = "0c4af863a7485bb99523c4f5ee978ec6699d78e3";
    sha256 = "16af1qmx7srhvcc936x7hl2bz50hafm39311dbzqam9ms1i5q89j";
  }