{
  description = "Development shell and package for llmchat-effectful";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;  # needed because openai is marked broken
        };
        haskellPackages = pkgs.haskell.packages.ghc9103.override {
          overrides = self: super: {
            openai = pkgs.haskell.lib.dontCheck super.openai;
          };
        };
        llmchat-effectful = haskellPackages.callCabal2nix "llmchat-effectful" ./. { };
        libraryPath = pkgs.lib.makeLibraryPath [
          pkgs.libpq
          pkgs.zlib
        ];
      in
      {
        packages.default = llmchat-effectful;

        devShells.default = haskellPackages.shellFor {
          packages = _: [ llmchat-effectful ];
          withHoogle = false;

          nativeBuildInputs = [
            pkgs.pkg-config
          ];

          buildInputs = [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.implicit-hie
            pkgs.fish
            pkgs.fourmolu
            pkgs.hlint
            pkgs.libpq
            pkgs.postgresql
            pkgs.zlib
          ];

          shellHook = ''
            export SHELL="${pkgs.fish}/bin/fish"
            export PKG_CONFIG_PATH="${pkgs.libpq.dev}/lib/pkgconfig:${pkgs.zlib.dev}/lib/pkgconfig:''${PKG_CONFIG_PATH:-}"
            export LD_LIBRARY_PATH="${libraryPath}:''${LD_LIBRARY_PATH:-}"
            exec "${pkgs.fish}/bin/fish"
          '';
        };
      });
}
