{

  network = {
   enableRollback = true;
  };

  server = { config, pkgs, lib, ... }:

  {
    require = [
      ./modules/homepage.nix
      ./modules/subsonic.nix
      ./modules/radicale.nix
      ./users.nix
    ];

    nixpkgs.config.allowUnfree = true;

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
    system.autoUpgrade.channel = https://nixos.org/channels/nixos-19.09;
    system.autoUpgrade.dates = "04:00";
    system.autoUpgrade.allowReboot = true;
    systemd.services.nixos-upgrade.environment.NIXOS_CONFIG = pkgs.writeText "configuration.nix" ''
      all@{ config, pkgs, lib, ... }: lib.filterAttrs (n: v: n != "deployment") ((import /etc/nixos/current/default.nix).server all)
    '';

    system.activationScripts = {
      configuration = ''
        rm /etc/nixos/current/* #*/
        ln -s ${./.}/* /etc/nixos/current #*/
      '';
    };

    # Work around NixOS/nixpkgs#28527
    systemd.services.nixos-upgrade.path = with pkgs; [  gnutar xz.bin gzip config.nix.package.out ];

    networking.hostName = "nixos"; # Define your hostname.

    networking.interfaces.ens3.ipv6.addresses =[
      { address = "2a03:4000:2:70e::42"; prefixLength = 64; }
    ];
    networking.defaultGateway6 = { address = "fe80::1"; interface = "ens3"; };

    networking.firewall.rejectPackets = true;
    networking.firewall.allowPing = true;
    networking.firewall.autoLoadConntrackHelpers = false;
    networking.firewall.allowedTCPPorts = [
      80 # http
      443 # https
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
    ];

    # Logrotate
    services.logrotate.enable = true;

    # FIXME: Integrate rotation into services.postgresqlBackup
    services.logrotate.config = ''
    /var/backup/postgresql/homepage_production.sql.gz {
      size 1K
      copytruncate
      rotate 100
    }
    '';

    # Enable the OpenSSH daemon
    services.openssh.enable = true;
    services.openssh.allowSFTP = true;
    services.openssh.forwardX11 = false;
    services.openssh.permitRootLogin = "yes"; # For deployment via NixOps
    services.openssh.passwordAuthentication = false;
    services.openssh.challengeResponseAuthentication = false;

    # PostgreSQL.
    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql95;
    services.postgresql.dataDir = "/var/lib/postgresql/9.5";
    services.postgresql.superUser = "postgres";
    services.postgresql.authentication = lib.mkForce ''
    # Generated file; do not edit!
    # TYPE  DATABASE        USER            ADDRESS                 METHOD
    local   all             all                                     trust
    host    all             all             127.0.0.1/32            trust
    host    all             all             ::1/128                 trust
    '';
    services.postgresqlBackup.databases = [ "homepage_production" ];
    services.postgresqlBackup.enable = true;
    services.postgresqlBackup.location = "/var/backup/postgresql";
    services.postgresqlBackup.startAt = "*-*-* 02:15:00";

    # Caldav / Cardav
    services.radicale.enable = true;
    services.radicale.config = ''
      [server]
      hosts = 127.0.0.1:5232
      ssl = False
      
      [storage]
      filesystem_folder = /srv/radicale/collections
      hook = ${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m "Changes by "%(user)s && GIT_SSH_COMMAND='${pkgs.openssh}/bin/ssh -o StrictHostKeyChecking=no -i /srv/radicale/id_rsa' ${pkgs.git}/bin/git push origin)
    '';
    services.radicale.package = pkgs.radicale2;
    services.radicale.nginx.enable = true;
    services.radicale.nginx.hostname = "calendar.pascal-wittmann.de";

    # Subsonic
    services.subsonic.enable = true;
    services.subsonic.defaultMusicFolder = "/srv/music";
    services.subsonic.defaultPlaylistFolder = "/srv/playlists";
    services.subsonic.defaultPodcastFolder = "/srv/podcast";
    services.subsonic.httpsPort = 0;
    services.subsonic.listenAddress = "127.0.0.1";
    services.subsonic.nginx.enable = true;
    services.subsonic.nginx.hostname = "music.pascal-wittmann.de";

    # nextcloud
    services.nextcloud.enable = true;
    services.nextcloud.home = "/srv/nextcloud";
    services.nextcloud.config.adminpassFile = "${./secrets/nextcloud}";
    services.nextcloud.hostName = "cloud.pascal-wittmann.de";
    services.nextcloud.nginx.enable = true;
    services.nextcloud.https = true;
    services.nextcloud.config =  {
      dbtype = "pgsql";
      dbport = 5432;
      dbname = "nextcloud";
      dbuser = "nextcloud";
      dbpassFile = "${./secrets/database-nextcloud}";
      dbhost = "127.0.0.1";
    };

    # nginx
    services.nginx.enable = true;
    services.nginx.virtualHosts = {
       "penchy.pascal-wittmann.de" = {
         forceSSL = true;
         enableACME = true;
         root = "/srv/penchy";
       };

       "cloud.pascal-wittmann.de" = {
         forceSSL = true;
         enableACME = true;
       };

       "netdata.pascal-wittmann.de" = {
         forceSSL = true;
         enableACME = true;
         locations."/" = { proxyPass = "http://127.0.0.1:19999"; };
         extraConfig = ''
           proxy_set_header X-Forwarded-Host $host;
           proxy_set_header X-Forwarded-Server $host;
           proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
           proxy_http_version 1.1;
           proxy_pass_request_headers on;
           proxy_set_header Connection "keep-alive";
           proxy_store off;

           auth_basic "Password protected area";
           auth_basic_user_file ${./secrets/passwords};
         '';
       };

       "users.pascal-wittmann.de" = {
         forceSSL = true;
         enableACME = true;

         locations."/pascal" = {
           root = "/srv/users/";
           extraConfig = ''
             autoindex on;
           '';
         };

         locations."/lerke" = {
           root = "/srv/users/";
           extraConfig = ''
             autoindex on;
             auth_basic "Password protected area";
             auth_basic_user_file ${./secrets/passwords};
           '';
         };
         extraConfig = ''
           add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
           add_header X-Content-Type-Options nosniff;
           add_header X-XSS-Protection "1; mode=block";
           add_header X-Frame-Options DENY;
         '';
       };
    };

    # Homepage
    services.homepage.enable = true;

    # Netdata
    services.netdata.enable = true;

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
