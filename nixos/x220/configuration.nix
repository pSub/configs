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
MIID1zCCAr+gAwIBAgIUJdXu3fQRPXyMvZzQIXsRXIpDmGQwDQYJKoZIhvcNAQEL
BQAwezELMAkGA1UEBhMCREUxEDAOBgNVBAgMB0hhbWJ1cmcxEDAOBgNVBAcMB0hh
bWJ1cmcxEjAQBgNVBAoMCWZyaXR6LmJveDERMA8GA1UEAwwIZml0ei5ib3gxITAf
BgkqhkiG9w0BCQEWEnBhc2NhbEBmcmV5LmZhbWlseTAeFw0yNDA3MjIyMTQxMTRa
Fw0yNDA4MjEyMTQxMTRaMHsxCzAJBgNVBAYTAkRFMRAwDgYDVQQIDAdIYW1idXJn
MRAwDgYDVQQHDAdIYW1idXJnMRIwEAYDVQQKDAlmcml0ei5ib3gxETAPBgNVBAMM
CGZpdHouYm94MSEwHwYJKoZIhvcNAQkBFhJwYXNjYWxAZnJleS5mYW1pbHkwggEi
MA0GCSqGSIb3DQEBAQUAA4IBDwAwggEKAoIBAQDHWdPVWR6pAwGtd0D4atqb4Wzh
wiqez7qtTDkH7jUY3KUg4+SmIrYfyRlLxz+Y/zgPtfKR5PYeezFc/CP7LD67su/o
dIfCYmKeVa4OzJ2K3O6xA5ntOr7LTEUO4L6nU+10XsOwrghYfpEYaSgRsr5zQJA8
yaYw0ak074By1mzv+jnC78D7RSISzak6PpkHowkzURpKfwX08hwmvJjFaNFjS76t
RqhsnILjaQWFCcZQuG+82RlFgVcRMKxLeFcvKVpaBNvrqO73u5doqk4mIA7O5t1K
Iv7u8NxzV/XT+e+lDPMDJvYBuQ5g3RPSQXtS+DPgkNhRH05Gt25QpPjsuqz3AgMB
AAGjUzBRMB0GA1UdDgQWBBTVVXBjwqhUma5ZOPeeyD87K6eqazAfBgNVHSMEGDAW
gBTVVXBjwqhUma5ZOPeeyD87K6eqazAPBgNVHRMBAf8EBTADAQH/MA0GCSqGSIb3
DQEBCwUAA4IBAQBPtduTW3c7Og7YSFfbYNVbZyO4ikQ/Vj9hRfgEsK2RJaAXgpbU
Oz/cKzqcqRXUA94+54r4aJxqy1NljVKhQOJijjlfhcveE5kw2kVX/zvzWZv1WEvJ
htwJM+HTZQRwtV5r8p/JLXImKlPjeXKOVzvvWnIyqxCOc8/rFxSKOYI3AH8Uobja
QZ9kCM0c4IveXeKEbAn6DoLrQ5vKfSCQ7+ZspyR8QYiCa0ycsPTHxzqc8SkfnGcB
PXXg2VN1AN9QqCQox8Rki3p74ATVuzGc7t5228bG2NmTZ3YcE0SduRTQObxmBYSf
4tRD5pu2v2kzsTx2DiiJJ5m/4Kvdudzebfsq
-----END CERTIFICATE-----
    ''
  ];

  # Enable sound
  security.rtkit.enable = true;
  sound.enable = false;
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
