{
  description = "NixOS configuration";

  inputs.nixpkgs.url = "nixpkgs/nixos-24.11";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";
  inputs.sops-nix.url = "github:Mic92/sops-nix";
  inputs.sops-nix.inputs.nixpkgs.follows = "nixpkgs";

  outputs = inputs @ { nixpkgs, nixos-hardware, sops-nix, ... }:
    let system = "x86_64-linux"; in {
      nixosConfigurations = {
        x220 = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = { inherit inputs; };
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-x220
            ./my-hardware-configuration.nix
            ./configuration.nix
            sops-nix.nixosModules.sops
          ];
        };
      };
    };
}
