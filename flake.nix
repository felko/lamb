{
  description = "A small functional programming language";
  
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    crate2nix = {
      url = "github:kolloch/crate2nix";
      flake = false;
    };
    flake-utils = {
      url = "github:numtide/flake-utils";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, crate2nix, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        inherit (import "${crate2nix}/tools.nix" { inherit pkgs; })
          generatedCargoNix;

        project = pkgs.callPackage (generatedCargoNix {
          name = "lamb";
          src = ./.;
        }) {
          defaultCrateOverrides = pkgs.defaultCrateOverrides // {
            # Crate dependency overrides go here
          };
        };

      in {
        packages.lamb = project.rootCrate.build;

        defaultPackage = self.packages.${system}.lamb;

        devShell = pkgs.mkShell {
          inputsFrom = builtins.attrValues self.packages.${system};
          buildInputs = [ pkgs.cargo pkgs.rust-analyzer pkgs.clippy ];
        };
      });
}

