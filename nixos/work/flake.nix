{
  description = "NixOS configuration";

  inputs.nixpkgs.url = "nixpkgs/nixos-25.05";
  inputs.unstable.url = "nixpkgs/nixos-unstable";

  outputs =
    inputs@{ nixpkgs, unstable, ... }:
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
            ./hardware-configuration.nix
            ./configuration.nix
          ];
        };
      };
    };
}
