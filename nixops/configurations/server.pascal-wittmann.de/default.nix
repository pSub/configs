{

  network = {
    enableRollback = true;
  };

  server = { config, pkgs, lib, ... }:

    {
      require = [
        ./modules/clean-deployment-keys.nixops.nix
        ./modules/homepage.nix
        ./modules/subsonic.nix
        ./modules/radicale.nix
        ./modules/dmn-check-server.nix
        ./modules/systemd-email-notify.nix
        ./users.nix
      ];

      nixpkgs.config.allowUnfree = true;

      deployment.targetHost = "server.pascal-wittmann.de";

      # Use the GRUB 2 boot loader.
      boot.loader.grub.enable = true;
      boot.loader.grub.version = 2;
      # Define on which hard drive you want to install Grub.
      boot.loader.grub.device = "/dev/vda";

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "uhci_hcd"
        "virtio_pci"
        "virtio_blk"
      ];
      boot.kernelModules = [];
      boot.extraModulePackages = [];

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


      # Deploy without root (enable with next nixops release)
      #nix.trustedUsers = [ "deployer" ];
      #users.users.deployer.extraGroups = [ "wheel" ];
      #security.sudo.wheelNeedsPassword = false;
      #deployment.targetUser = "deployer";

      systemd.email-notify.mailTo = "mail@pascal-wittmann.de";
      systemd.email-notify.mailFrom = "systemd <admin@frey.family>";

      system.autoUpgrade.enable = true;
      system.autoUpgrade.channel = https://nixos.org/channels/nixos-20.03;
      system.autoUpgrade.dates = "04:00";
      system.autoUpgrade.allowReboot = true;
      systemd.services.nixos-upgrade.environment.NIXOS_CONFIG = pkgs.writeText "configuration.nix" ''
        all@{ config, pkgs, lib, ... }:
              with lib; with builtins;
              let
                modifyPaths = f : paths : map toPath (map f (map toString paths));

                serverConfig = (import /etc/nixos/current/default.nix).server all;
                withoutDeploymentOptions = removeAttrs serverConfig [ "deployment" ];
                withoutDeploymentRequires = overrideExisting withoutDeploymentOptions
                                             { require = modifyPaths (require: concatStrings [ "/etc/nixos/current/" (replaceStrings [ "nix/store/"] [ "" ] require) ])
                                                                             (filter (filename: ! (hasInfix ".nixops" (toString filename)))
                                                                                              serverConfig.require);

                                               system = serverConfig.system // {
                                                 activationScripts = removeAttrs serverConfig.system.activationScripts [ "copy-configuration" ];
                                               };
                                             };
              in withoutDeploymentRequires
      '';

      system.activationScripts = {
        copy-configuration = ''
          if [ -d /etc/nixos/current ]; then
             rm -r /etc/nixos/current
          fi
          mkdir /etc/nixos/current

          ln -s ${./default.nix} /etc/nixos/current/default.nix
          ln -s ${./users.nix} /etc/nixos/current/users.nix
          ln -s ${./modules} /etc/nixos/current/modules
        '';
      };

      # Work around NixOS/nixpkgs#28527
      systemd.services.nixos-upgrade.path = with pkgs; [ gnutar xz.bin gzip config.nix.package.out ];

      networking.hostName = "nixos"; # Define your hostname.

      networking.interfaces.ens3.ipv6.addresses = [
        { address = "2a03:4000:2:70e::42"; prefixLength = 64; }
      ];
      networking.defaultGateway6 = { address = "fe80::1"; interface = "ens3"; };

      networking.firewall.enable = true;
      networking.firewall.allowPing = true;
      networking.firewall.autoLoadConntrackHelpers = false;
      networking.firewall.allowedTCPPorts = [
        80 # http
        443 # https
      ];

      # Select internationalisation properties.
      i18n.defaultLocale = "en_US.UTF-8";

      console = {
        font = "Lat2-Terminus16";
        keyMap = "de";
      };

      # Set your time zone.
      time.timeZone = "Europe/Berlin";

      # Security - PAM
      security.pam.loginLimits = [
        {
          domain = "*";
          item = "maxlogins";
          type = "-";
          value = "3";
        }
      ];

      security.acme.email = "contact@pascal-wittmann.de";
      security.acme.acceptTerms = true;


      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages = with pkgs; [
        # Install only the urxvt terminfo file
        rxvt_unicode.terminfo
        zile
      ];

      # List services that you want to enable:

      services.ssmtp.enable = true;
      services.ssmtp.domain = "frey.family";
      services.ssmtp.hostName = "frey-family.netcup-mail.de:587";
      services.ssmtp.root = "admin@frey.family";
      services.ssmtp.useSTARTTLS = true;
      services.ssmtp.authUser = "admin@frey.family";
      services.ssmtp.authPassFile = "/var/keys/smtp";

      # Cron daemon.
      services.cron.enable = true;
      services.cron.systemCronJobs = [];

      # Logrotate
      services.logrotate.enable = true;

      # FIXME: Integrate rotation into services.postgresqlBackup
      services.logrotate.config = ''
        /var/backup/postgresql/homepage_production.sql.gz {
          rotate 100
          missingok
        }
        /var/backup/postgresql/nextcloud.sql.gz {
          rotate 100
          missingok
        }

        /var/spool/nginx/logs/*.log {
          daily
          missingok
          rotate 31
          compress
          delaycompress
          notifempty
          sharedscripts
          postrotate
                [ -f /run/nginx/nginx.pid ] && kill -USR1 `cat /run/nginx/nginx.pid`
          endscript
        }
      '';

      # Enable the OpenSSH daemon
      services.openssh.enable = true;
      services.openssh.allowSFTP = true;
      services.openssh.forwardX11 = false;
      services.openssh.permitRootLogin = "yes"; # For deployment via NixOps, non-root deployments via NixOS/nixops#730
      services.openssh.passwordAuthentication = false;
      services.openssh.challengeResponseAuthentication = false;

      # PostgreSQL.
      services.postgresql.enable = true;
      services.postgresql.package = pkgs.postgresql_11;
      services.postgresql.dataDir = "/var/lib/postgresql/9.11";
      services.postgresql.superUser = "postgres";
      services.postgresqlBackup.databases = [ "homepage_production" "nextcloud" ];
      services.postgresqlBackup.enable = true;
      services.postgresqlBackup.location = "/var/backup/postgresql";
      services.postgresqlBackup.startAt = "*-*-* 02:15:00";

      # MySQL
      services.mysql.enable = true;
      services.mysql.package = pkgs.mysql;

      # Caldav / Cardav
      services.radicale.enable = true;
      services.radicale.config = ''
        [server]
        hosts = 127.0.0.1:5232
        ssl = False
      
        [storage]
        filesystem_folder = /srv/radicale/collections
        hook = ${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m "Changes by "%(user)s && GIT_SSH_COMMAND='${pkgs.openssh}/bin/ssh -o StrictHostKeyChecking=no -i /srv/radicale/id_rsa' ${pkgs.git}/bin/git push origin)

        [auth]
        type = htpasswd
        htpasswd_filename = /var/keys/radicale
        # encryption method used in the htpasswd file
        htpasswd_encryption = bcrypt
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
      services.nextcloud.package = pkgs.nextcloud18;
      services.nextcloud.home = "/srv/nextcloud";
      services.nextcloud.config.adminpassFile = "/var/keys/nextcloud";
      services.nextcloud.hostName = "cloud.pascal-wittmann.de";
      services.nextcloud.nginx.enable = true;
      services.nextcloud.https = true;
      services.nextcloud.autoUpdateApps.enable = true;
      services.nextcloud.config = {
        dbtype = "pgsql";
        dbport = 5432;
        dbname = "nextcloud";
        dbuser = "nextcloud";
        dbpassFile = "/var/keys/databaseNextcloud";
        dbhost = "127.0.0.1";
      };

      # bitwarden_rs
      services.bitwarden_rs.enable = true;
      services.bitwarden_rs.backupDir = "/var/backup/bitwarden";
      services.bitwarden_rs.config = {
        domain = "https://bitwarden.pascal-wittmann.de:443";
        rocketPort = 8222;
        signupsAllowed = false;
      };

      # nginx
      services.nginx.enable = true;
      services.nginx.commonHttpConfig = ''
        map $remote_addr $ip_anonym1 {
        default 0.0.0;
        "~(?P<ip>(\d+)\.(\d+))\.(\d+)\.\d+" $ip;
        "~(?P<ip>[^:]+:[^:]+):" $ip;
        }

        map $remote_addr $ip_anonym2 {
        default .0.0;
        "~(?P<ip>(\d+)\.(\d+)\.(\d+))\.\d+" .0.0;
        "~(?P<ip>[^:]+:[^:]+):" ::;
        }

        map $ip_anonym1$ip_anonym2 $ip_anonymized {
        default 0.0.0.0;
        "~(?P<ip>.*)" $ip;
        }

        log_format anonymized '$ip_anonymized - $remote_user [$time_local] '
        '"$request" $status $body_bytes_sent '
        '"$http_referer" "$http_user_agent"';

        access_log /var/spool/nginx/logs/access.log anonymized;
      '';
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

        "bitwarden.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = { proxyPass = "http://localhost:8222"; };
          extraConfig = ''
            add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
            add_header X-Content-Type-Options nosniff;
            add_header X-XSS-Protection "1; mode=block";
            add_header X-Frame-Options SAMEORIGIN;
          '';
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
            auth_basic_user_file /var/keys/basicAuth;
          '';
        };

        "notes.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = { proxyPass = "http://127.0.0.1:3456/"; };
          extraConfig = ''
            add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
            add_header X-Content-Type-Options nosniff;
            add_header X-XSS-Protection "1; mode=block";
            add_header X-Frame-Options SAMEORIGIN;
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
              auth_basic_user_file /var/keys/basicAuth;
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

      # dmn-check-server
      services.dmn-check-server.enable = true;

      # Netdata
      services.netdata.enable = true;

      # Tiddlywiki
      services.tiddlywiki.enable = true;
      services.tiddlywiki.listenOptions = {
        credentials = "/var/keys/tiddlywiki";
        port = 3456;
        readers = "(authenticated)";
        writers = "(authenticated)";
      };

      # Sound
      sound.enable = false;

      # Enable zsh
      programs.zsh.enable = true;

      # X-libraries and fonts are not needed on the server.
      #  environment.noXlibs = true;
      fonts.fontconfig.enable = false;

      users.mutableUsers = false;
      users.defaultUserShell = "${pkgs.zsh}/bin/zsh";

      deployment.keys.nextcloud.text = builtins.readFile ./secrets/nextcloud;
      deployment.keys.nextcloud.destDir = "/var/keys";
      deployment.keys.nextcloud.user = "nextcloud";

      deployment.keys.databaseNextcloud.text = builtins.readFile ./secrets/database-nextcloud;
      deployment.keys.databaseNextcloud.destDir = "/var/keys";
      deployment.keys.databaseNextcloud.user = "nextcloud";

      deployment.keys.basicAuth.text = builtins.readFile ./secrets/passwords;
      deployment.keys.basicAuth.destDir = "/var/keys";
      deployment.keys.basicAuth.user = "nginx";

      deployment.keys.smtp.text = builtins.readFile ./secrets/smtp;
      deployment.keys.smtp.destDir = "/var/keys";
      deployment.keys.smtp.group = "mail";

      deployment.keys.databaseHomepage.text = builtins.readFile ./secrets/homepage_database_password;
      deployment.keys.databaseHomepage.destDir = "/var/keys";
      deployment.keys.databaseHomepage.user = "homepage";

      deployment.keys.radicale.text = builtins.readFile ./secrets/radicale;
      deployment.keys.radicale.destDir = "/var/keys";
      deployment.keys.radicale.user = "radicale";

      deployment.keys.tiddlywiki.text = builtins.readFile ./secrets/tiddlywiki;
      deployment.keys.tiddlywiki.destDir = "/var/keys";
      deployment.keys.tiddlywiki.user = "tiddlywiki";
    };
}
