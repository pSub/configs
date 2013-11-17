# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

let

  # Displays an alert if the battery is below 10%
  lowBatteryNotifier = pkgs.writeScript "lowBatteryNotifier"
    ''
      DISPLAY=:0.0
      BAT=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '[0-9]+(?=%)'`
      test $BAT -le 10 && DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Low Battery'
    '';

in {
  require =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
    ];

  boot.kernelPackages = pkgs.linuxPackages_3_10;

  boot.initrd.kernelModules =    [
      # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
    ];

  boot.initrd.luks.cryptoModules = ["aes" "sha256" "sha1" "cbc"];

  # This should be move to the hardware configuration file
  boot.kernelModules = [ "tp_smapi" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.tp_smapi ];

  boot.kernelParams = [ "quiet" ];
    
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;

  # Set LUKS device
  boot.initrd.luks.devices = [
    { name = "luksroot"; 
      device = "/dev/sda2"; 
      preLVM = true; 
    }
  ];

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/sda";

  networking.hostName = "brauchli"; # Define your hostname.
  networking.networkmanager.enable = true;

  # Power Management
  powerManagement.cpuFreqGovernor = "conservative";

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

  users.extraUsers.pascal = {
    createHome = true;
    createUser = true;
    description = "Pascal Wittmann";
    group = "users";
    extraGroups = [ "wheel" ];
    home = "/home/pascal";
    shell = "/var/run/current-system/sw/bin/zsh";
  };

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  fonts = {
    enableFontDir = true;
    enableCoreFonts = true;
    enableGhostscriptFonts = true;
    extraFonts = with pkgs ; [
      liberation_ttf
      ttf_bitstream_vera
      dejavu_fonts
      terminus_font
      bakoma_ttf
      clearlyU
      cm_unicode
      andagii
      bakoma_ttf
      inconsolata
      gentium
    ];
  };

  # List services that you want to enable:

  # Cron.
  services.cron.enable = true;
  services.cron.mailto = "root";
  services.cron.systemCronJobs = [
    "30 23 * * * pascal DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Time to go to bed'"
    "* * * * *   pascal ${lowBatteryNotifier}"
  ];

  # Udisks.
  services.udisks.enable = true;

  # Acpi.
  services.acpid.enable = true;
  services.acpid.lidEventCommands = ''
    LID="/proc/acpi/button/lid/LID/state"
    state=`cat $LID | ${pkgs.gawk}/bin/awk '{print $2}'`
    case "$state" in
      *open*) ;;
      *close*) ${pkgs.pmutils}/sbin/pm-suspend ;;
      *) logger -t lid-handler "Failed to detect lid state ($state)" ;;
    esac
  '';

  # ClamAV.
  services.clamav.updater.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;

  # SMART.
  services.smartd.enable = true;
  services.smartd.devices = [ { device = "/dev/sda"; } ];

  # Thinkfan
  services.thinkfan.enable = true;
  services.thinkfan.sensor = "/sys/devices/virtual/hwmon/hwmon0/temp1_input";

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbVariant = "nodeadkeys";
  services.xserver.synaptics.enable = true;
  services.xserver.synaptics.accelFactor = "0.01";
  services.xserver.synaptics.tapButtons = false;
  services.xserver.synaptics.vertEdgeScroll = true;
  services.xserver.displayManager.slim.enable = true;
  services.xserver.displayManager.slim.autoLogin = true;
  services.xserver.displayManager.slim.defaultUser = "pascal";
  services.xserver.startGnuPGAgent = true;
  services.xserver.startOpenSSHAgent = false;
  services.xserver.videoDrivers = [ "intel" "vesa" ];
  services.xserver.vaapiDrivers = [ pkgs.vaapiIntel ];

  # MPD
  services.mpd.enable = true;

  # Firewall
  #networking.firewall.enable = true;

  # Time.
  time.timeZone = "Europe/Berlin";

  environment = {
    systemPackages = with pkgs; [
      atool
      dunst
      unzip
      unison
      git
      htop
      inotifyTools
      transmission_remote_gtk
      wmname
      zsh

      # Emacs
      emacs24
      emacs24Packages.org
      emacs24Packages.autoComplete
      emacs24Packages.haskellMode
      emacs24Packages.magit
      emacs24Packages.scalaMode

      # Xutils
      xclip
      xbindkeys
      xlibs.xinput

      # Misc
      haskellPackages.Agda
      haskellPackages.AgdaExecutable
    ];

    etc = {
      # This link is used to esablish compatibility to Arch.
      certificates = {
        source = "${pkgs.cacert}/etc/ca-bundle.crt";
        target = "ssl/certs/ca-certificates.crt";
      };
    };
  };
}
