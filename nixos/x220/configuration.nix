# Edit this configuration file to define what should be installed on
# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8).

{ pkgs, config, ... }:
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
  sops.defaultSopsFile = ./secrets/secrets.yaml;
  sops.defaultSopsFormat = "yaml";
  sops.age.keyFile = "/home/pascal/.config/sops/age/keys.txt";

  sops.secrets."users/pascal".neededForUsers = true;
  sops.secrets."users/root".neededForUsers = true;
  sops.secrets.restic = {};

  system.stateVersion = "23.05";

  nixpkgs.config.allowUnfree = true;

  services.flatpak.enable = true;
  xdg.portal.enable = true;

  # Trust hydra. Needed for one-click installations.
  nix.settings.trusted-substituters = [ "http://hydra.nixos.org" ];
  nix.settings.trusted-public-keys = [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
  ];
  nix.settings.trusted-users = [ "root" "pascal" ];

  # Build using chroots to detect more impurities.
  nix.settings.sandbox = true;

  nix.settings.experimental-features = [ "nix-command" "flakes" ];

  hardware.sane.enable = true;
  hardware.sane.extraBackends = [ pkgs.hplipWithPlugin ];

  # Use the network manager.
  networking.networkmanager.enable = true;

  security.pki.certificates = [
    # fritz.box certificate
    # Howto generate the certificate:
    # https://stackoverflow.com/questions/59738140/why-is-firefox-not-trusting-my-self-signed-certificate
    ''
-----BEGIN CERTIFICATE-----
MIID2TCCAsGgAwIBAgIUV+AmONoiSGfWf0qXGNXtmcDu4A0wDQYJKoZIhvcNAQEL
BQAwfDELMAkGA1UEBhMCREUxEDAOBgNVBAgMB0hhbWJ1cmcxEDAOBgNVBAcMB0hh
bWJ1cmcxEjAQBgNVBAoMCWZyaXR6LmJveDESMBAGA1UEAwwJZnJpdHouYm94MSEw
HwYJKoZIhvcNAQkBFhJwYXNjYWxAZnJleS5mYW1pbHkwHhcNMjQwOTA4MjEwNDUy
WhcNMjUwOTA4MjEwNDUyWjB8MQswCQYDVQQGEwJERTEQMA4GA1UECAwHSGFtYnVy
ZzEQMA4GA1UEBwwHSGFtYnVyZzESMBAGA1UECgwJZnJpdHouYm94MRIwEAYDVQQD
DAlmcml0ei5ib3gxITAfBgkqhkiG9w0BCQEWEnBhc2NhbEBmcmV5LmZhbWlseTCC
ASIwDQYJKoZIhvcNAQEBBQADggEPADCCAQoCggEBAKNiBr5gOjOrehDhecrqjKY1
07XtUKebelnCQru778O9wP1k08Er3p3IqBZX3X7lPhjujG7I8rRb9dti96r764wN
QK1qHLSRgVWlK58FLNTtafCRGDgWWokMZgscCNaExq45ET1vkeRueqqFmE+S1n2x
ok9WvGkwxbNhaId/68rENtwizBkivhK7Y52a2Nrr0aDu+Z5Z4M8LFtHG71G8flup
1iIpdWPbrCiqjr+6rSBJNRXcSJGN1XV1OudRGO/CgkaXnzqDEaSYQR6qauD5vmh4
ajG+C/eOzq8+b22qlnXMWkH9kOx+Q0lQSsXnGb3q9C65SChps5aP4jYJWw1HRPcC
AwEAAaNTMFEwHQYDVR0OBBYEFNtpN+8XrZa8NyWSs2s0VKLBuF3TMB8GA1UdIwQY
MBaAFNtpN+8XrZa8NyWSs2s0VKLBuF3TMA8GA1UdEwEB/wQFMAMBAf8wDQYJKoZI
hvcNAQELBQADggEBAJ+1KtIZ3p9GGF3C2RIGwRcwg6dh+slZfkJVi2drE90Olw3i
oVkGMOb0O3j7GgnTdAuL+BDsEJB/+JqkJzWMeFdhxcH4kwrMcBcha8axvpFDtNRs
MthNcb1hy8xnHQfH9im/KWBhPfAqRjsG5BSCSO9wJJZPf7hSA36J8RAdTqbU20++
lHR5OxutPlrSENTe84iYWK6UVDzYNtkgjE6Ji6s6ZdOcm/CTJ9fBb/tKmnY8kdF+
NAffCEj+y0PYuOY1aYij0o9UPoByfWAWTMF5gEqxXnuFkOMeuyNCz6oS+16U1v5i
skRh4A2bjJHJGz2Z/EoAj4wcZzmXT+jMnWZtGnA=
-----END CERTIFICATE-----
    ''
  ];

  # Enable sound
  security.rtkit.enable = true;
  services.pipewire = {
    enable = true;
    alsa.enable = true;
    alsa.support32Bit = true;
    pulse.enable = true;
    jack.enable = true;
    wireplumber.enable = true;
  };

  hardware.bluetooth.enable = true;
  services.blueman.enable = true;

  virtualisation.docker.enable = true;

  users.mutableUsers = false;
  users.users.root.hashedPasswordFile = config.sops.secrets."users/root".path;
  users.extraUsers.pascal = {
    uid = 1002;
    description = "Pascal Wittmann";
    extraGroups = [ "networkmanager" "vboxusers" "lp" "scanner" "wheel" "docker" ];
    isNormalUser = true;
    hashedPasswordFile = config.sops.secrets."users/pascal".path;
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
    packages = with pkgs ; [
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

  services.restic.backups.cloud-backup = {
    repository = "sftp://u388595.your-storagebox.de:23/x220";
    paths = [ "/home" ];
    exclude = [
      "*.tmp"
      "*.temp"
      "/home/*/.cache"
      "/home/pascal/audiobooks"
      "/home/pascal/pictures"
      "/home/pascal/nixpkgs"
      "/home/pascal/go"
      "/home/pascal/studium"
      "/home/pascal/calibre"
      ".git"
    ];
    timerConfig = null;
    passwordFile = config.sops.secrets.restic.path;
    initialize = true;
  };

  services.resolved = {
    enable = true;
    dnssec = "true";
    domains = [ "~." ]; # "use as default interface for all requests"
    extraConfig = ''
        DNSOverTLS=opportunistic
        MulticastDNS=resolve
    '';
    llmnr = "true";
  };

  networking.nameservers = [
    "152.53.0.129#adguard.pascal-wittmann.de"
  ];

  networking.hosts = {
    "192.168.178.1" = [ "fritz.box" ];
  };

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

  # Mopidy
  services.mopidy.enable = true;
  services.mopidy.extensionPackages = with pkgs; [ mopidy-mpd mopidy-subidy ];
  services.mopidy.extraConfigFiles = [ "/etc/nixos/mopidy-subidy.conf" ];

  # Enable the X11 windowing system.
  services.xserver.enable = true;
  services.xserver.xkb.layout = "de";
  services.xserver.xkb.variant = "nodeadkeys";
  services.xserver.xkb.options = "";
  services.libinput.enable = false;
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
  services.displayManager.defaultSession = "none+xmonad";

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

  # Do not start ssh-agent, keyring is used.
  programs.ssh.startAgent = false;

  #
  programs.kdeconnect.enable = true;

  programs.thunar.plugins = with pkgs.xfce; [
    thunar-archive-plugin
  ];

  # Firefox with PWAs
  programs.firefox = {
    enable = true;
    package = pkgs.firefox;
    nativeMessagingHosts.packages = [ pkgs.firefoxpwa ];
  };

  # Firewall
  networking.firewall.enable = true;

  # Time.
  time.timeZone = "Europe/Berlin";

  environment = {
    systemPackages = with pkgs; [
      exfat
      dconf
      bashInteractive
      firefoxpwa
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
