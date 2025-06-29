{ config, pkgs, modulesPath, ... }:

{
  require = [
    (modulesPath + "/installer/scan/not-detected.nix")
  ];

  networking.hostName = "brauchli";

  boot.kernelPackages = pkgs.linuxPackages;

  boot.loader.grub.configurationLimit = 10;
  boot.initrd.kernelModules = [ "ehci_hcd" "ahci" "xhci_hcd" "usb_storage" ];
  boot.initrd.availableKernelModules = [ "cbc" "hmac" "sha256" "rng" "aes" "encrypted_keys" ];

  # System has i7-2640M thus only the aes module is needed for luks.
  boot.initrd.luks.cryptoModules = [ "aes" "hmac" "rng" "encrypted_keys" ];

  # Set LUKS device.
  boot.initrd.luks.devices.luksroot = {
    device = "/dev/disk/by-id/ata-WD_Green_2.5_1000GB_251439800584-part2";
    preLVM = true;
  };

  boot.kernelModules = [ "kvm-intel" "tp_smapi" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];

  # Disable beep.
  boot.blacklistedKernelModules = [ "pcspkr" ];

  boot.kernelParams = [ "quiet" ];

  # Use the GRUB boot loader.
  boot.loader.grub.enable = true;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/disk/by-id/ata-WD_Green_2.5_1000GB_251439800584";
  #boot.loader.grub.device = "/dev/sda1";

  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.
  fileSystems."/".device = "/dev/mapper/vg0-root";
  fileSystems."/boot".device = "/dev/disk/by-id/ata-WD_Green_2.5_1000GB_251439800584-part1";
  fileSystems."/tmp" = { device = "tmpfs"; fsType = "tmpfs"; };

  nix.settings.max-jobs = 4;

  services.xserver.videoDrivers = [ "modesetting" "vesa" ];
  hardware.graphics.extraPackages = [ pkgs.vaapiIntel ];
  hardware.graphics.enable32Bit = true;

  # Power Management
  powerManagement.cpuFreqGovernor = "conservative";

  hardware.trackpoint.enable = true;
  hardware.trackpoint.sensitivity = 180;
  hardware.trackpoint.speed = 180;
}
