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
    [ # Include settings that depend on specific hardware.
      ./my-hardware-configuration.nix
      ./password.nix
    ];

  # Trust hydra. Needed for one-click installations.
  nix.trustedBinaryCaches = [ "http://hydra.nixos.org" ];

  networking.networkmanager.enable = true;

  # Power Management
  powerManagement.cpuFreqGovernor = "conservative";

  users.mutableUsers = false;
  users.extraUsers.pascal = {
    uid = 499;
    createHome = true;
    createUser = true;
    description = "Pascal Wittmann";
    group = "users";
    extraGroups = [ "networkmanager" ];
    home = "/home/pascal";
    shell = "/var/run/current-system/sw/bin/zsh";
  };
  fileSystems."/home/pascal/downloads" = { device = "tmpfs" ; fsType = "tmpfs"; options = "size=25%"; };
  fileSystems."/home/pascal/sandbox" = { device = "tmpfs" ; fsType = "tmpfs"; options = "size=25%"; };

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

  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql92;

  # Cron.
  services.cron.enable = true;
  services.cron.mailto = "root";
  services.cron.systemCronJobs = [
    "30 23 * * * pascal DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Time to go to bed'"
    "* * * * *   pascal ${lowBatteryNotifier}"
    "@weekly     pascal ${pkgs.coreutils}/bin/touch $HOME/.backup-weekly"
  ];

  # Gernal Purpose Mouse.
  services.gpm.enable = true;

  # Udisks.
  services.udisks.enable = true;

  # Rsnapshot.
  services.rsnapshot.enable = true;
  services.rsnapshot.extraConfig =
    ''
    snapshot_root	/backup/x220
    no_create_root	1
    retain	weekly	52
    backup	/home/	.
    backup	/etc/nixos/	.
    backup	/var/	.
    '';

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

  services.xserver.windowManager.xmonad.enable = true;
  services.xserver.windowManager.xmonad.enableContribAndExtras = true;
  services.xserver.windowManager.xmonad.extraPackages = haskellPackages : [
	];
  services.xserver.windowManager.default = "xmonad";

  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.desktopManager.default = "none";

  services.xserver.startGnuPGAgent = true;
  services.xserver.startOpenSSHAgent = false;

  # MPD
  services.mpd.enable = true;

  system.copySystemConfiguration = true;

  # Firewall
  networking.firewall.enable = true;

  # Time.
  time.timeZone = "Europe/Berlin";

  environment = {
    systemPackages = with pkgs; [
      zsh
    ];

    etc = {
      # This link is used to esablish compatibility to Arch.
      certificates = {
        source = "${pkgs.cacert}/etc/ca-bundle.crt";
        target = "ssl/certs/ca-certificates.crt";
      };
    };

    variables = {
      # Make XCompose work in GTK applications
      GTK_IM_MODULE = "xim";
    };
  };
}
