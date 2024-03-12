{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs = { self, nixpkgs, sops-nix }:
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
        modules = [
          ./configuration.nix
          sops-nix.nixosModules.sops
        ];
      };
    };
}