# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ config, pkgs, ... }:

let

  # Displays an alert if the battery is below 10%
  lowBatteryNotifier = pkgs.writeScript "lowBatteryNotifier"
    ''
      BAT_PCT=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '[0-9]+(?=%)'`
      BAT_STA=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '\w+(?=,)'`
      test $BAT_PCT -le 10 && test $BAT_STA = "Discharging" && DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Low Battery'
    '';

in {
  require =
    [ # Include settings that depend on specific hardware.
      /etc/nixos/my-hardware-configuration.nix

      # Include the file with the hashed passwords. Ensure
      # that permissions are set correctly.
      /etc/nixos/password.nix
    ];

  nixpkgs.config.allowUnfree = true;


  # Trust hydra. Needed for one-click installations.
  nix.trustedBinaryCaches = [ "http://hydra.nixos.org" ];
  nix.binaryCachePublicKeys = [ "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
                                "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs=" ];

  # Build using chroots to detect more impurities.
  nix.useSandbox = true;

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplipWithPlugin ];

  # Use the network manager.
  networking.networkmanager.enable = true;

  # Enable sound
  sound.enable = true;

  users.mutableUsers = false;
  users.extraUsers.pascal = {
    uid = 1002;
    description = "Pascal Wittmann";
    extraGroups = [ "networkmanager" "vboxusers" "lp" "scanner" ];
    isNormalUser = true;
    shell = "${pkgs.zsh}/bin/zsh";
  };
  fileSystems."/home/pascal/downloads" = { device = "tmpfs" ; fsType = "tmpfs"; options = [ "size=25%" ]; };
  fileSystems."/home/pascal/sandbox" = { device = "tmpfs" ; fsType = "tmpfs"; options = [ "size=25%" ]; };

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

  # Cron.
  services.cron.enable = true;
  services.cron.mailto = "pascal";
  services.cron.systemCronJobs = [
    "30 23 * * * pascal DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Time to go to bed'"
    "* * * * *   pascal ${lowBatteryNotifier}"
    "@weekly     root   nix-collect-garbage"
  ];

  # Gernal Purpose Mouse.
  services.gpm.enable = true;

  # Udisks.
  services.udisks2.enable = true;

  # Tor.
  services.tor.client.enable = true;

  # Acpi.
  services.acpid.enable = true;

  services.logind.lidSwitch = "suspend";

  # ClamAV.
  services.clamav.daemon.enable = true;
  services.clamav.updater.enable = true;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  # SMART.
  services.smartd.enable = true;
  services.smartd.devices = [ { device = "/dev/sda"; } ];

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
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.extraSeatDefaults = ''
    autologin-user = pascal
  '';

  services.xserver.windowManager.xmonad = { enable = true;
                                            enableContribAndExtras = true;
                                          };
  services.xserver.windowManager.default = "xmonad";
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.desktopManager.xfce = {
    enable = true;
    enableXfwm = false;
    noDesktop = true;
    thunarPlugins = with pkgs.xfce; [
      thunar-archive-plugin
    ];
  };

  services.urxvtd.enable = true;

  # Copy the system configuration int to nix-store.
  system.copySystemConfiguration = true;

  # Do not start ssh-agent, gnupg-agent is used.
  programs.ssh.startAgent = false;

  # Firewall
  networking.firewall.enable = true;

  # DNSCrypt
  services.dnscrypt-proxy.enable = true;
  services.dnscrypt-proxy.customResolver = {
    address = "167.86.90.103";
    port = 443;
    name = "2.dnscrypt-cert.dnscrypt.me";
    key = "325f:58cb:c543:5ddf:2d08:8f32:2d4e:29d2:2fc1:9a72:e7e0:7efb:9f7e:ade0:1a8f:9bc5";
  };
  networking.nameservers = ["127.0.0.1"];

  # Time.
  time.timeZone = "Europe/Berlin";

  environment = {
    systemPackages = with pkgs; [
      exfat-utils
      gnome3.dconf
      bashInteractive
      lsof
      zile
    ];

    variables = {
      # Make XCompose work in GTK applications.
      GTK_IM_MODULE = "xim";
    };
  };

  services.tlp.enable = true;

  services.dbus.enable = true;
}
