{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  inputs.flake-utils.url = "github:numtide/flake-utils/v1.0.0";

  outputs =
    {
      self,
      flake-utils,
      nixpkgs,
      ...
    }:

    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          config.allowBroken = true;
        };
        hsenv = pkgs.haskellPackages.ghcWithPackages (p: [ ]);
      in
      {
        devShells = {
          default = pkgs.mkShell {
            name = "script-shell";
            buildInputs = [
              hsenv
              pkgs.nixfmt-rfc-style
            ];
          };
        };
        packages = {
          ci_script = import ./ci_script.nix {
            inherit (pkgs) ghc stdenv;
          };
        };
      }
    );
}
