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
        name = "squeal-boilerplate";
        ghc = "ghc964";
        haskellPackages = pkgs.haskell.packages.${ghc}.override {
          overrides = hp: super:
            let
              hlib = pkgs.haskell.lib;
              jailbreakUnbreak = pkg: hlib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));
            in {
              ${name} = hp.callCabal2nix name (fs.toSource {
                root = ./.;
                fileset = fs.unions [ ./src ./${name}.cabal ];
              }) {};
              singletons-base = hlib.dontCheck (jailbreakUnbreak super.singletons-base);
              # 2024-03-22 marked as broken in nixpkgs
              # also has broken hspec dep
              records-sop = hlib.dontCheck (hp.callHackage "records-sop" "0.1.1.1" {});
            };
        };

      in {
        packages = {
          default = haskellPackages.${name};
        };
        defaultPackage = self.packages.${system}.default;
        devShells.default = pkgs.mkShell {
          buildInputs =
            let
              hask = with haskellPackages; [
                haskell-language-server
                fourmolu
                cabal-fmt
                stan
                ghcid
                cabal-install
                hlint
              ];
              shell = with pkgs; [ treefmt just ];
            in
              hask ++ shell;
          inputsFrom = map (e: (e.env or {})) (__attrValues self.packages.${system});
        };
        devShell = self.devShells.${system}.default;
      });
}
