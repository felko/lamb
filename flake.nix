{
  description = "A small functional programming language";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    fenix = {
      url = "github:nix-community/fenix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    naersk = {
      url = "github:nmattia/naersk";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, fenix, naersk, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        toolchain = fenix.packages.${system}.complete;

        naersk-lib = naersk.lib.${system}.override {
          inherit (toolchain) cargo rustc;
        };

        lamb = naersk-lib.buildPackage {
          name = "lamb";
          src = ./.;
        };
      in {
        packages.lamb = lamb;
        defaultPackage = self.packages.${system}.lamb;

        devShell = pkgs.mkShell {
          inputsFrom = builtins.attrValues self.packages.${system};
          nativeBuildInputs = [
            (toolchain.withComponents [
              "cargo" "rustc" "rust-src" "rustfmt" "clippy"
            ])
          ];
          RUST_BACKTRACE = 1;
        };
      });
}
