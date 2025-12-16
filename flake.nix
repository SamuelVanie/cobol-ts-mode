{
  description = "COBOL major mode with tree-sitter support";

  inputs = {
    emacs-package-flake.url = "github:mrkkrp/emacs-package-flake";
  };

  outputs = { self, emacs-package-flake }:
    emacs-package-flake.lib.mkFlake {
      src = ./.;
      packageName = "cobol-ts-mode";
    };
}
