{ config, pkgs, ... }:

{
  require = [
    <nixos/modules/installer/scan/not-detected.nix>
  ];

  networking.hostName = "brauchli";

  boot.kernelPackages = pkgs.linuxPackages;
  boot.initrd.kernelModules = [ "ehci_hcd" "ahci" "xhci_hcd" "usb_storage" ];

  # System has i7-2640M thus only the aes module is needed for luks.
  boot.initrd.luks.cryptoModules = [ "aes" ];

  # Set LUKS device.
  boot.initrd.luks.devices = [
    { name = "luksroot";
      device = "/dev/disk/by-id/wwn-0x500a07510338a9bf-part2"; #sda2
      preLVM = true;
    }
  ];

  boot.kernelModules = [ "kvm-intel" "tp_smapi" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];

  # Disable beep.
  boot.blacklistedKernelModules = [ "pcspkr" ];

  boot.kernelParams = [ "quiet" ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/disk/by-id/wwn-0x500a07510338a9bf";

  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.
  fileSystems."/".device = "/dev/mapper/vgroup-root";
  fileSystems."/boot".device = "/dev/disk/by-id/wwn-0x500a07510338a9bf-part1";
  fileSystems."/tmp" = { device = "tmpfs"; fsType = "tmpfs"; };

  # List swap partitions activated at boot time.
  swapDevices =
    [ { device = "/dev/mapper/vgroup-swap"; }
    ];

  nix.maxJobs = 4;

  services.xserver.videoDrivers = [ "intel" "vesa" ];
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
  hardware.opengl.driSupport32Bit = true;

  # Power Management
  powerManagement.cpuFreqGovernor = "conservative";

  hardware.trackpoint.enable = true;
  hardware.trackpoint.sensitivity = 180;
  hardware.trackpoint.speed = 180;
}
