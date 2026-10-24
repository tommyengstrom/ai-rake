{
  description = "airake libraries and development shell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        names = [ "airake-core" "airake-chat" "airake-openrouter" "airake-openai" "airake-jev" "airake-postgres" "airake" ];
        haskellPackages = pkgs.haskell.packages.ghc9103.override {
          overrides = final: prev: builtins.listToAttrs (map (name: {
            inherit name;
            value = final.callCabal2nix name (./packages + "/${name}") { };
          }) names);
        };
        libraries = builtins.listToAttrs (map (name: { inherit name; value = haskellPackages.${name}; }) names);
      in {
        packages = libraries // { default = libraries.airake; };
        devShells.default = haskellPackages.shellFor {
          packages = p: map (name: p.${name}) names;
          withHoogle = false;
          nativeBuildInputs = [ pkgs.pkg-config ];
          buildInputs = [
            haskellPackages.cabal-install
            haskellPackages.ghcid
            haskellPackages.haskell-language-server
            haskellPackages.hspec-discover
            haskellPackages.implicit-hie
            pkgs.fish
            pkgs.fourmolu
            pkgs.hlint
            pkgs.libpq
            pkgs.postgresql
            pkgs.process-compose
            pkgs.zlib
            pkgs.ffmpeg
          ];
          shellHook = ''
            export PC_PORT_NUM=6599
            export LD_LIBRARY_PATH=${pkgs.lib.makeLibraryPath [ pkgs.libpq pkgs.zlib ]}:''${LD_LIBRARY_PATH:-}
          '';
        };
      });
}
