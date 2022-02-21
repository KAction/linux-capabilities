{ compiler ? "ghc901" }:

let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "linux-capabilities" =
        hself.callCabal2nix
          "linux-capabilities"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."linux-capabilities"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.ormolu
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.niv
      pkgs.nixpkgs-fmt
    ];
    withHoogle = true;
    shellHook = "hpack";
  };
in
{
  inherit shell;
  inherit myHaskellPackages;
  "linux-capabilities" = myHaskellPackages."linux-capabilities";
}
