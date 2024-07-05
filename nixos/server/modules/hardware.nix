{ lib, modulesPath, ... }:

{
  imports =
    [
      (modulesPath + "/profiles/qemu-guest.nix")
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "virtio_pci" "virtio_scsi" "ahci" "usbhid" "sr_mod" "cryptd" "igb" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "virtio-pci" ];
  boot.extraModulePackages = [ ];

   fileSystems."/" =
    {
      device = "none";
      fsType = "tmpfs";
      options = [ "defaults" "size=1G" "mode=0755" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/D60D-1312";
      fsType = "vfat";
      options = [ "umask=0077" ];
    };

  fileSystems."/nix" =
    {
      device = "/dev/disk/by-uuid/12bd4453-c5df-4f27-bfcb-483099ab38dc";
      fsType = "ext4";
    };


  boot.initrd.luks.devices."cryptroot".device = "/dev/disk/by-uuid/dfb8a329-404f-4c38-9637-959165f66bc0";

  # Mount whole /tmp on disk as long as it is not possible to modify the build dir of nixos-rebuild
  # see https://github.com/NixOS/nixpkgs/issues/293114
  fileSystems."/tmp" =
    {
      device = "/nix/persist/tmp";
      fsType = "none";
      options = [ "bind" ];
   };

  fileSystems."/etc/nixos" =
    {
      device = "/nix/persist/etc/nixos";
      fsType = "none";
      options = [ "bind" ];
    };

  fileSystems."/home" =
    {
      device = "/nix/persist/home";
      fsType = "none";
      options = [ "bind" ];
    };

  fileSystems."/var" =
    {
      device = "/nix/persist/var";
      fsType = "none";
      options = [ "bind" ];
    };

  fileSystems."/srv" =
    {
      device = "/nix/persist/srv";
      fsType = "none";
      options = [ "bind" ];
    };

  fileSystems."/srv/pictures" = {
      device = "//u388595.your-storagebox.de/u388595-sub2";
      fsType = "cifs";
      options = let
        # this line prevents hanging on network split
        automount_opts = "x-systemd.automount,noauto,x-systemd.idle-timeout=60,x-systemd.device-timeout=5s,x-systemd.mount-timeout=5s";

      in ["${automount_opts},credentials=/run/secrets/cifs/pictures,uid=nextcloud,gid=nextcloud"];
    };

  swapDevices = [ ];
  
  nixpkgs.hostPlatform = lib.mkDefault "aarch64-linux";
}
  
