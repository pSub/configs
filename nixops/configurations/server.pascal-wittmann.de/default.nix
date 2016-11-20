{

  server = { pkgs, ... }:

  let acmeWebRoot = "/srv/acme";
      /*monitorCode = (import <nixpkgs> {}).fetchFromGitHub {
        owner = "pSub";
        repo = "nixpkgs-monitor";
        rev = "2fb8f4a3d9af4d9dd5349fa10f81afe725da688c";
        sha256 = "1q089mz7x9gaif7zfz3hj2dbsqmmbdl7fj5nw1iwn7zahvv9kc37";
      };*/
      monitorCode = /home/pascal/projects/nixpkgs-monitor;
  in {

    require = [
      ./modules/homepage.nix
      ./modules/subsonic.nix
      ./modules/radicale.nix
      ./modules/h5ai.nix
      ./modules/nixpkgs-monitor.nix
      ./users.nix

      "${monitorCode}/service.nix"
    ];

    deployment.targetHost = "server.pascal-wittmann.de";

    # Use the GRUB 2 boot loader.
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
    # Define on which hard drive you want to install Grub.
    boot.loader.grub.device = "/dev/vda";

    boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci"
      "virtio_blk" ];
    boot.kernelModules = [ ];
    boot.extraModulePackages = [ ];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/7d067332-eba7-4a8e-acf7-a463cf50677f";
      fsType = "ext4";
    };

    swapDevices = [
      { device = "/dev/disk/by-uuid/279e433e-1ab9-4fd1-9c37-0d7e4e082944"; }
    ];

    nix.maxJobs = 2;
    nix.gc.automatic = true;
    nix.gc.dates = "06:00";

    system.autoUpgrade.enable = true;
    system.autoUpgrade.channel = https://nixos.org/channels/nixos-16.03;
    systemd.services.nixos-upgrade.environment.NIXOS_CONFIG = pkgs.writeText "configuration.nix" ''
      all@{ lib, ... }: lib.filterAttrs (n: v: n != "deployment") ((import /etc/nixos/current/default.nix).server all)
    '';

    system.activationScripts = {
      configuration = ''
        rm /etc/nixos/current/* #*/
        ln -s ${./.}/* /etc/nixos/current #*/
      '';
    };

    networking.hostName = "nixos"; # Define your hostname.

    networking.firewall.rejectPackets = true;
    networking.firewall.allowPing = true;
    networking.firewall.autoLoadConntrackHelpers = false;
    networking.firewall.allowedTCPPorts = [
      80 # http
      443 # https
      4242 # quassel
    ];

    # Select internationalisation properties.
    i18n = {
      consoleFont = "Lat2-Terminus16";
      consoleKeyMap = "de";
      defaultLocale = "en_US.UTF-8";
    };

    # Set your time zone.
    time.timeZone = "Europe/Berlin";

    # Security - PAM
    security.pam.loginLimits = [ {
      domain = "*";
      item = "maxlogins";
      type = "-";
      value = "3";
    } ];

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      # Install only the urxvt terminfo file
      rxvt_unicode.terminfo
      zile
    ];

  
    # List services that you want to enable:

    # Cron daemon.
    services.cron.enable = true;
    services.cron.systemCronJobs = [
      "30 2 * * * root start nixpkgs-monitor-updater"
    ];

    # Enable the OpenSSH daemon
    services.openssh.enable = true;
    services.openssh.allowSFTP = true;
    services.openssh.forwardX11 = false;
    services.openssh.permitRootLogin = "yes"; # For deployment via NixOps
    services.openssh.passwordAuthentication = false;
    services.openssh.challengeResponseAuthentication = false;

    # bitlbee.
    services.bitlbee.enable = true;
    services.bitlbee.interface = "127.0.0.1";
    services.bitlbee.portNumber = 6667;
    services.bitlbee.authMode = "Registered";
    services.bitlbee.hostName = "server.pascal-wittmann.de";
    services.bitlbee.configDir = "/srv/bitlbee";

    # quassel
    services.quassel.enable = true;
    services.quassel.interfaces = [ "0.0.0.0" ];
    services.quassel.dataDir = "/srv/quassel";

    # PostgreSQL.
    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql92;

    # Caldav / Cardav
    services.radicale.enable = true;
    services.radicale.config = ''
      [server]
      hosts = 127.0.0.1:5232
      daemon = True
      ssl = False
      base_prefix = /radicale/
      
      [storage]
      filesystem_folder = /srv/radicale/collections
    '';
    services.lighttpd.radicale.enable = true;
    services.lighttpd.radicale.hostname = "pascal-wittmann.de";

    services.subsonic.enable = true;
    services.subsonic.defaultMusicFolder = "/srv/music";
    services.subsonic.defaultPlaylistFolder = "/srv/playlists";
    services.subsonic.defaultPodcastFolder = "/srv/podcast";
    services.subsonic.httpsPort = 0;
    services.subsonic.listenAddress = "127.0.0.1";
    services.subsonic.contextPath = "/music";
    services.lighttpd.subsonic.enable = true;
    services.lighttpd.subsonic.hostname = "pascal-wittmann.de";

    # lighttpd
    services.lighttpd.enable = true;
    services.lighttpd.document-root = "/srv/www/";
    services.lighttpd.port = 80;
    services.lighttpd.enableModules = [ "mod_redirect" "mod_proxy"
      "mod_userdir" "mod_auth" ];
    services.lighttpd.h5ai.enable = true;
    services.lighttpd.extraConfig =  ''
      server.follow-symlink = "enable"
      
      auth.debug = 2
      auth.backend = "plain"
      auth.backend.plain.userfile = "${./secrets/passwords}"

      userdir.basepath = "/srv/users/"
      userdir.path = ""
      userdir.include-user = ( "pascal", "qwert", "lerke" )

      dir-listing.activate = "enable"
      dir-listing.encoding = "utf-8"
      $SERVER["socket"] == ":443" {
        ssl.engine                  = "enable"
        ssl.pemfile                 = "/srv/homepage/ssl/www.pascal-wittmann.de.pem"
        ssl.ca-file                 = "/srv/homepage/ssl/ca.crt"
        ssl.cipher-list = "${import static/ssl-cipher-list.txt}"
        ssl.dh-file = "/srv/homepage/ssl/dhparams.pem"
      }

      setenv.add-response-header = (
        "Strict-Transport-Security" => "max-age=63072000; includeSubDomains; preload",
        "X-Content-Type-Options" => "nosniff",
        "X-Frame-Options" => "DENY",
        "X-XSS-Protection" => "1; mode=block",
        "Public-Key-Pins" => "pin-sha256=\"aiHvkTqXNmsZ9V78XaIbP6VHV5O2Q1oN85+N/r3qATA=\"; pin-sha256=\"ZjOx5W+YxpIcqzuFaFr4o0yXxxu1QrUhIq5NFpdy9zY=\"; max-age=5184000; includeSubdomains"
      )
    '';

    # Homepage
    services.homepage.enable = true;

    # Nixpkgs Monitor
    services.nixpkgs-monitor.enable = true;
    services.nixpkgs-monitor.baseUrl = "https://pascal-wittmann.de/nixpkgs-monitor/";
    services.lighttpd.nixpkgs-monitor.enable = true;
    services.lighttpd.nixpkgs-monitor.hostname = "pascal-wittmann.de";
#    services.nixpkgs-monitor.host = "0.0.0.0";

    # Sound
    sound.enable = false;

    # Enable zsh
    programs.zsh.enable = true;

    # X-libraries and fonts are not needed on the server.
    #  environment.noXlibs = true;
    fonts.fontconfig.enable = false;

    users.mutableUsers = false;
    users.defaultUserShell = "${pkgs.zsh}/bin/zsh";
  };
}
