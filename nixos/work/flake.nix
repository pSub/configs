{
  description = "NixOS configuration";

  inputs.nixpkgs.url = "nixpkgs/nixos-25.05";
  inputs.unstable.url = "nixpkgs/nixos-unstable";
  inputs.nixos-hardware.url = "github:NixOS/nixos-hardware/master";

  outputs =
    inputs@{
      nixpkgs,
      unstable,
      nixos-hardware,
      ...
    }:
    let
      system = "x86_64-linux";
    in
    {
      nixosConfigurations = {
        work = nixpkgs.lib.nixosSystem {
          inherit system;
          specialArgs = {
            unstable = import unstable { inherit system; };
          };
          modules = [
            nixos-hardware.nixosModules.lenovo-thinkpad-p1-gen3
            ./hardware-configuration.nix
            ./configuration.nix
          ];
        };
      };
    };
}
