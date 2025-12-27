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


  # IP:<ignore>:GATEWAY:NETMASK:HOSTNAME:NIC:AUTCONF?
  # See: https://www.kernel.org/doc/Documentation/filesystems/nfs/nfsroot.txt
  boot.kernelParams = [
    "ip=152.53.0.129::152.53.0.1:152.53.3.255:v22024034028258810.nicesrv.de:enp3s0:off"
    "debugfs=off"
  ];
  networking = {
    useDHCP = false;
    interfaces."enp3s0" = {
      ipv4.addresses = [{ address = "152.53.0.129"; prefixLength = 22; }];
      ipv6.addresses = [{ address = "2a0a:4cc0:0:10a5::1"; prefixLength = 64; }];
    };
    defaultGateway = "152.53.0.1";
    defaultGateway6 = { address = "fe80::1"; interface = "enp3s0"; };
  };

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

