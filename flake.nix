{
  description = "Cobol treesitter implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
        };
      in
        {
          packages.default = pkgs.stdenv.mkDerivation {
            pname = "emacs.cobol-ts-mode";
            version = "0.1.0";
            src = ./.;
            
            installPhase = ''
              mkdir -p $out
              cp *.el $out/
            '';
          };
        });
}
