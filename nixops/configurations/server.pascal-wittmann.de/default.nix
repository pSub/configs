{

  server = { pkgs, ... }:

  {

    require = [
      ./modules/homepage.nix
      ./users.nix

      #/home/pascal/projects/nixpkgs-monitor/service.nix
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
    system.autoUpgrade.enable = true;
    system.autoUpgrade.channel = https://nixos.org/channels/nixos-16.03;

    networking.hostName = "nixos"; # Define your hostname.

    networking.firewall.rejectPackets = true;
    networking.firewall.allowPing = true;
    networking.firewall.autoLoadConntrackHelpers = false;
    networking.firewall.allowedTCPPorts = [
      80 # http
      443 # https
      6667 # bitlbee
      4242 # quassel
      5232 # radicale
      4567 # nixpkgs-monitor
      5000 # lint-review
      5672 # celery
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

    # Enable the OpenSSH daemon
    services.openssh.enable = true;
    services.openssh.allowSFTP = true;
    services.openssh.forwardX11 = false;
    services.openssh.permitRootLogin = "yes"; # For deployment via NixOps
    services.openssh.passwordAuthentication = false;
    services.openssh.challengeResponseAuthentication = false;

    # bitlbee.
    services.bitlbee.enable = true;
    services.bitlbee.interface = "0.0.0.0";
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
      hosts = server.pascal-wittmann.de:5232
      daemon = True
      ssl = True
      protocol = PROTOCOL_SSLv23
      certificate = ${./secrets/cert.pem}
      key = ${./secrets/key.pem}
      base_prefix = /
      
      [storage]
      filesystem_folder = /srv/radicale/collections
      
      [auth]
      type = htpasswd
      htpasswd_filename = ${./secrets/passwords}
      htpasswd_encryption = plain
    '';

    services.subsonic.enable = true;
    services.subsonic.defaultMusicFolder = "/srv/music";
    services.subsonic.defaultPlaylistFolder = "/srv/playlists";
    services.subsonic.defaultPodcastFolder = "/srv/podcast";
    services.subsonic.httpsPort = 0;
    services.subsonic.listenAddress = "127.0.0.1";
    #services.subsonic.lighttpd.enable = false;
    #services.subsonic.lighttpd.hostname = "music.pascal-wittmann.de";
    #services.subsonic.lighttpd.pemFile = "/srv/homepage/ssl/www.pascal-wittmann.de.pem";
    #services.subsonic.lighttpd.caFile = "/srv/homepage/ssl/ca.crt";

    #services.lighttpd.enableModules = [ "mod_proxy" ];
    #services.lighttpd.extraConfig = ''
    #  $HTTP["host"] == "music.pascal-wittmann.de" {
    #    $SERVER["socket"] == ":443" {
    #      ssl.engine                  = "enable"
    #      ssl.pemfile                 = "/srv/homepage/ssl/www.pascal-wittmann.de.pem"
    #      ssl.ca-file                 = "/srv/homepage/ssl/ca.crt"
    #    }
    #    proxy.balance = "hash"
    #    proxy.server  = ( "" => (( "host" => "127.0.0.1", "port" => ${toString services.subsonic.port} )))
    #  }
    #'';

    # lighttpd
    services.lighttpd.enable = true;
    services.lighttpd.document-root = "/srv/www/";
    services.lighttpd.port = 80;
    services.lighttpd.enableModules = [ "mod_redirect" "mod_proxy" "mod_fastcgi"
      "mod_userdir" "mod_auth" ];
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

      $HTTP["host"] == "music.pascal-wittmann.de" {
        $SERVER["socket"] == ":443" {
          ssl.engine                  = "enable"
          ssl.pemfile                 = "/srv/homepage/ssl/www.pascal-wittmann.de.pem"
          ssl.ca-file                 = "/srv/homepage/ssl/ca.crt"
        }
        proxy.balance = "hash"
        proxy.server  = ( "" => (( "host" => "127.0.0.1", "port" => 4040 )))
      }
    '';

    # Homepage
    services.homepage.enable = true;

    # Nixpkgs Monitor
#    services.nixpkgs-monitor.enable = true;
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
