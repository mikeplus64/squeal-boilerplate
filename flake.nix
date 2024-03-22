{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:gytis-ivaskevicius/flake-utils-plus";
  };
  outputs = inputs@{ self, nixpkgs, flake-utils, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; overlays = [ flake-utils.overlay ]; };
        fs = pkgs.lib.fileset;
        ghc = "ghc948";
        thisPackage = "squeal-boilerplate";
        haskellPackages = pkgs.haskell.packages.${ghc}.override {
          overrides = hp: super: {
            ${thisPackage} = hp.callCabal2nix thisPackage (fs.toSource {
              root = ./.;
              fileset = fs.unions [ ./src ./test (fs.fileFilter (file: file.ext == "cabal")) ];
            }) {};
          };
        };
      in {
        packages = {
          default = haskellPackages.${thisPackage};
        };
        defaultPackage = self.packages.${system}.default;
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            hp.haskell-language-server
            hp.fourmolu
            hp.cabal-fmt
            hp.stan
            hp.ghcid
            hp.cabal-install
            hp.hlint
            treefmt
            just
          ];
          inputsFrom = map (e: (e.env or {})) (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
