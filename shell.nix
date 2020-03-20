{ pkgs ? import <nixpkgs> { } }:
let
  haskellDotNix = pkgs.fetchFromGitHub {
    owner = "input-output-hk";
    repo = "haskell.nix";
    rev = "33dc7bce3444f8957dec9dccb484dd4b40ee463d";
    sha256 = "0ffv3sfjkba4b565gpgs0qznd2jh5v60ql03dkc5bgx2sh8yfhsv";
  };
  nix-tools = (import pkgs.path (import haskellDotNix)).haskell-nix.nix-tools;
in
  pkgs.mkShell {
    buildInputs = [
      pkgs.stack
      nix-tools
    ];
  }
