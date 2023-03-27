# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ pkgs, ... }:
let
  # Displays an alert if the battery is below 10%
  lowBatteryNotifier = pkgs.writeScript "lowBatteryNotifier"
    ''
      BAT_PCT=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '[0-9]+(?=%)'`
      BAT_STA=`${pkgs.acpi}/bin/acpi -b | ${pkgs.gnugrep}/bin/grep -P -o '\w+(?=,)'`
      test $BAT_PCT -le 10 && test $BAT_STA = "Discharging" && DISPLAY=:0.0 ${pkgs.libnotify}/bin/notify-send 'Low Battery'
    '';
in
{
  require =
    [
      # Use X220 configuration from https://github.com/NixOS/nixos-hardware
      <nixos-hardware/lenovo/thinkpad/x220>

      # Include settings that depend on specific hardware.
      /etc/nixos/my-hardware-configuration.nix

      # Include the file with the hashed passwords. Ensure
      # that permissions are set correctly.
      /etc/nixos/password.nix
    ];

  system.stateVersion = "22.05";

  nixpkgs.config.allowUnfree = true;

  # Trust hydra. Needed for one-click installations.
  nix.settings.trusted-substituters = [ "http://hydra.nixos.org" ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
  ];

  # Build using chroots to detect more impurities.
  nix.settings.sandbox = true;

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
  fileSystems."/home/pascal/downloads" = { device = "tmpfs"; fsType = "tmpfs"; options = [ "size=25%" ]; };
  fileSystems."/home/pascal/sandbox" = { device = "tmpfs"; fsType = "tmpfs"; options = [ "size=25%" ]; };

  i18n.defaultLocale = "en_US.UTF-8";

  console = {
    font = "lat9w-16";
    keyMap = "de";
  };

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs ; [
      corefonts
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


  # KDE Connect
  networking.firewall = {
    allowedTCPPortRanges = [ { from = 1714; to = 1764; } ];
    allowedUDPPortRanges = [ { from = 1714; to = 1764; } ];
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

  # Gnome Keyring
  services.gnome.gnome-keyring.enable = true;

  # Udisks.
  services.udisks2.enable = true;

  # Tor.
  services.tor.client.enable = true;

  # Acpi.
  services.acpid.enable = true;

  services.logind.lidSwitch = "suspend";

  # ClamAV.
  services.clamav.daemon.enable = false;
  services.clamav.updater.enable = false;

  # Enable CUPS to print documents.
  services.printing.enable = true;
  services.printing.drivers = [ pkgs.hplip ];

  # SMART.
  services.smartd.enable = true;
  services.smartd.devices = [{ device = "/dev/sda"; }];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.layout = "de";
  services.xserver.xkbVariant = "nodeadkeys";
  services.xserver.xkbOptions = "";
  services.xserver.libinput.enable = false;
  services.xserver.synaptics = {
    enable = true;
    accelFactor = "0.01";
    tapButtons = false;
    vertEdgeScroll = true;
  };
  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.extraSeatDefaults = ''
    autologin-user = pascal
  '';
  services.xserver.displayManager.defaultSession = "none+xmonad";

  services.xserver.windowManager.xmonad = {
    enable = true;
    enableContribAndExtras = true;
  };
  services.xserver.desktopManager.xterm.enable = false;
  services.xserver.desktopManager.xfce = {
    enable = true;
    enableXfwm = false;
    noDesktop = true;
  };

  # Copy the system configuration int to nix-store.
  system.copySystemConfiguration = true;

  # gpg-agent
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gtk2";
  };

  # Do not start ssh-agent, gnupg-agent is used.
  programs.ssh.startAgent = false;

  #
  programs.kdeconnect.enable = true;

  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
  ];

  # Firewall
  networking.firewall.enable = true;

  # Time.
  time.timeZone = "Europe/Berlin";

  environment = {
    systemPackages = with pkgs; [
      exfat
      dconf
      bashInteractive
      lsof
      zile
    ];

    variables = {
      # Make XCompose work in GTK applications.
      GTK_IM_MODULE = "xim";
    };

    homeBinInPath = true;
  };

  services.tlp.enable = true;

  services.dbus.enable = true;
}
