{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.11";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.phare-nix.url = "github:pSub/phare-nix";
  inputs.phare-nix.inputs.nixpkgs.follows = "nixpkgs";
  inputs.homepage-nix.url = "github:pSub/pascal-wittmann.de";
  inputs.homepage-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { nixpkgs, sops-nix, phare-nix, homepage-nix, ... }:
    let
      system = "aarch64-linux";
      pkgs = nixpkgs.legacyPackages.${system};
    in
    {
      # I also use nix-direnv, so this ensures `nixos-rebuild` is
      # available in my shell when I cd into this folder.
      devShell."${system}" = pkgs.mkShell {
        packages = with pkgs; [ nixos-rebuild ];
      };

      nixosConfigurations.nixos = nixpkgs.lib.nixosSystem {
        inherit system;
        specialArgs = {
          pharePkgs = {
            docs = phare-nix.packages.${system}.phareNixDocs;
          };
          homepage-app = homepage-nix.packages.${system}.homepage;
        };
        modules = [
          ./modules/homepage.nix
          ./configuration.nix
          sops-nix.nixosModules.sops
          phare-nix.nixosModules.phare
        ];
      };
    };
}
