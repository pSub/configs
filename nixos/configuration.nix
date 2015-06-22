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

      # Include the file with the hashed passwords. Ensure
      # that permissions are set correctly.
      ./password.nix
    ];

  nixpkgs.config.allowUnfree = true;

  # Trust hydra. Needed for one-click installations.
  nix.trustedBinaryCaches = [ "http://hydra.nixos.org" ];

  # Build using chroots to detect more impurities.
  nix.useChroot = true;

  # Use the network manager.
  networking.networkmanager.enable = true;

  users.mutableUsers = false;
  users.extraUsers.pascal = {
    uid = 499;
    createHome = true;
    description = "Pascal Wittmann";
    group = "users";
    extraGroups = [ "networkmanager" "vboxusers" ];
    home = "/home/pascal";
    shell = "${pkgs.zsh}/bin/zsh";
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
    fonts = with pkgs ; [
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

  services.postgresql.enable = false;
  services.postgresql.package = pkgs.postgresql92;

  # Cron.
  services.cron.enable = true;
  services.cron.mailto = "pascal";
  services.cron.systemCronJobs = [
    "30 23 * * * pascal DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Time to go to bed'"
    "* * * * *   pascal ${lowBatteryNotifier}"
    "@weekly     pascal ${pkgs.coreutils}/bin/touch $HOME/.backup-weekly"
    "@weekly     root   nix-collect-garbage"
  ];

  # Gernal Purpose Mouse.
  services.gpm.enable = true;

  # Udisks.
  services.udisks2.enable = true;

  # Rsnapshot.
  services.rsnapshot.enable = true;
  services.rsnapshot.extraConfig =
    ''
    rsync_long_args	--progress
    snapshot_root	/backup/x220
    no_create_root	1
    retain	weekly	52
    backup	/home/	.
    backup	/etc/nixos/	.
    backup	/var/	.
    '';

  # Tor.
  services.tor.client.enable = true;

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
  services.xserver.xkbOptions = "";
  services.xserver.synaptics = { enable = true;
                                 accelFactor = "0.01";
                                 tapButtons = false;
                                 vertEdgeScroll = true;
                               };
  services.xserver.displayManager.slim = { enable = true;
                                           autoLogin = true;
                                           defaultUser = "pascal";
                                         };
  services.xserver.windowManager.xmonad = { enable = true;
                                            enableContribAndExtras = true;
                                            extraPackages = haskellPackages : [];
                                          };
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.xterm.enable = false;

  # Use GnuPG agent.
  services.xserver.startGnuPGAgent = true;

  # MPD
  services.mpd.enable = true;

  # Copy the system configuration int to nix-store.
  system.copySystemConfiguration = true;

  # Do not start ssh-agent, gnupg-agent is used.
  programs.ssh.startAgent = false;

  # Firewall
  networking.firewall.enable = true;

  # Time.
  time.timeZone = "Europe/Berlin";

  environment = {
    systemPackages = with pkgs; [
      exfat-utils
      gnome3.dconf
      bashInteractive
    ];

    variables = {
      # Make XCompose work in GTK applications.
      GTK_IM_MODULE = "xim";
    };
  };

  services.tlp.enable = true;

  services.dbus.enable = true;
}
