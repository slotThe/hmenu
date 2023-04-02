{
  description = "Nix flake for hmenu";
  inputs = {
    nixpkgs.url     = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { flake-utils, nixpkgs, self }:
    flake-utils.lib.eachDefaultSystem (system:
      let pkgs = import nixpkgs { inherit system; };
      in {
        packages.hmenu-exe = pkgs.haskellPackages.callCabal2nix "hmenu-exe" self {};
        defaultPackage     = self.packages.${system}."hmenu-exe";
      }
    );
}
