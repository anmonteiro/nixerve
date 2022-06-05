{
  inputs = {
    nixpkgs.url = "github:nixOS/nixpkgs";
    nix-filter.url = "github:numtide/nix-filter";
    flake-utils.url = "github:numtide/flake-utils";

    ocaml-overlay.url = "github:anmonteiro/nix-overlays";
    ocaml-overlay.inputs.nixpkgs.follows = "nixpkgs";
    ocaml-overlay.inputs.flake-utils.follows = "flake-utils";
  };

  outputs = { self, nixpkgs, nix-filter, flake-utils, ocaml-overlay }:
    with flake-utils.lib;
    eachSystem [ system.x86_64-linux ] (system:
      let
        pkgs = ocaml-overlay.makePkgs {
          inherit system;
        };

        nixerve = pkgs.callPackage ./nix/nixerve.nix { nix-filter = nix-filter.lib.filter; };
      in
      {
        devShell = pkgs.mkShell {
          inputsFrom = [ nixerve ];
          packages = with pkgs; with ocamlPackages; [
            ocamlformat
            ocaml
            findlib
            dune_3
            ocaml-lsp
          ];
        };
      });
}
