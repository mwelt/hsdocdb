# let nixpkgs = import <nixpkgs> {};
#     orig = nixpkgs.pkgs.haskellPackages.callPackage ./default.nix {};
# in (nixpkgs.pkgs.haskell.lib.doBenchmark orig).env
let nixpkgs = import <nixpkgs> {};
    prjPackages = import ./release.nix;
    projectDrvEnv = prjPackages.project.env.overrideAttrs (oldAttrs: rec {
      buildInputs = oldAttrs.buildInputs ++ [
        nixpkgs.haskellPackages.cabal-install
      ];
    });
in projectDrvEnv
