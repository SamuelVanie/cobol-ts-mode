{
  description = "Nix for cobol-ts-mode";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in
        {
          packages.default = pkgs.emacsPackages.trivialBuild {
            pname = "cobol-ts-mode";
            version = "0.1.0";
            src = ./.;
          };
        }
    );
}
