{
  description = "NixOS configuration";

  inputs.nixpkgs.url = "nixpkgs/nixos-25.05";
  inputs.red6.url = "git+ssh://git@github.com/red6/nixos-modules";

  outputs =
    inputs@{
      nixpkgs,
      red6,
      ...
    }:
    let
      system = "x86_64-linux";
    in
    {
      nixosConfigurations = {
        work = nixpkgs.lib.nixosSystem {
          inherit system;
          modules = [
            red6.nixosModules.${system}.hansemerkur-vpn
            red6.nixosModules.${system}.hansemerkur-certificates
            ./hardware-configuration.nix
            ./configuration.nix
          ];
        };
      };
    };
}
