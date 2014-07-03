{ config, pkgs, ... }:

{
  require = [
    <nixos/modules/installer/scan/not-detected.nix>
  ];

  networking.hostName = "brauchli";

  boot.kernelPackages = pkgs.linuxPackages_3_15;
  boot.initrd.kernelModules = [ "ehci_hcd" "ahci" "xhci_hcd" "usb_storage" ];

  # System has i7-2640M thus only the aes module is needed for luks.
  boot.initrd.luks.cryptoModules = [ "aes" ];

  # Set LUKS device.
  boot.initrd.luks.devices = [
    { name = "luksroot";
      device = "/dev/sda2";
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
  boot.loader.grub.device = "/dev/sda";

  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.
  fileSystems."/".device = "/dev/mapper/vgroup-root";
  fileSystems."/boot".device = "/dev/sda1";
  fileSystems."/tmp" = { device = "tmpfs"; fsType = "tmpfs"; };

  # List swap partitions activated at boot time.
  swapDevices =
    [ { device = "/dev/mapper/vgroup-swap"; }
    ];

  nix.maxJobs = 4;

  services.xserver.videoDrivers = [ "intel" "vesa" ];
  services.xserver.vaapiDrivers = [ pkgs.vaapiIntel ];

  # Power Management
  powerManagement.cpuFreqGovernor = "conservative";

  hardware.trackpoint.enable = true;
  hardware.trackpoint.sensitivity = 220;
  hardware.trackpoint.speed = 200;
}
