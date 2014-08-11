{ config, pkgs, ... }:

{
  require = [
    <nixos/modules/installer/scan/not-detected.nix>
  ];

  networking.hostName = "subiectiva";

  boot.kernelPackages = pkgs.linuxPackages_3_10;

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ata_piix" "ahci" ];
  boot.initrd.luks.cryptoModules = ["aes" "sha512" "sha1" "cbc" "xts"];

  # Set LUKS device
  boot.initrd.luks.devices = [
    { name = "luksroot";
      device = "/dev/sda2";
      preLVM = true;
    }
  ];

  boot.kernelModules = [ "tp_smapi" "thinkpad_acpi" ];
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

  nix.maxJobs = 2;

  services.xserver.videoDrivers = [ "intel" "vesa" ];

  # Power Management
  powerManagement.cpuFreqGovernor = "conservative"; 
}
