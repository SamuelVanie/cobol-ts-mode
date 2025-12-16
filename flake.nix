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
        cobol-ts-mode = pkgs.emacsPackages.trivialBuild {
          pname = "cobol-ts-mode";
          version = "0.1.0";
          src = self;
        };
      in
        {
          packages = {
            default = cobol-ts-mode;
            cobol-ts-mode = cobol-ts-mode;
          };
          
          # This makes it searchable by devbox
          legacyPackages = {
            cobol-ts-mode = cobol-ts-mode;
          };
        }
    );
}
