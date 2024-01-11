{

  network = {
    enableRollback = true;
    storage.legacy = { };
  };

  server = { config, pkgs, options, ... }:

    {
      require = [
        ./modules/clean-deployment-keys.nixops.nix
        ./modules/homepage.nix
        ./modules/radicale.nix
        ./modules/systemd-email-notify.nix
        ./users.nix
      ];

      nixpkgs.config.allowUnfree = true;

      deployment.targetHost = "server.pascal-wittmann.de";

      # Use the GRUB 2 boot loader.
      boot.loader.grub.enable = true;
      # Define on which hard drive you want to install Grub.
      boot.loader.grub.device = "/dev/vda";

      boot.initrd.availableKernelModules = [
        "ata_piix"
        "uhci_hcd"
        "virtio_pci"
        "virtio_blk"
      ];
      boot.kernelModules = [ ];
      boot.extraModulePackages = [ ];

      fileSystems."/" = {
        device = "/dev/disk/by-uuid/7d067332-eba7-4a8e-acf7-a463cf50677f";
        fsType = "ext4";
      };

      swapDevices = [
        { device = "/dev/disk/by-uuid/279e433e-1ab9-4fd1-9c37-0d7e4e082944"; }
      ];

      nix.settings.max-jobs = 2;
      nix.gc.automatic = true;
      nix.gc.dates = "06:00";


      # Deploy without root
      nix.settings.trusted-users = [ "deployer" ];
      security.sudo.wheelNeedsPassword = false;
      deployment.targetUser = "deployer";

      systemd.email-notify.mailTo = "mail@pascal-wittmann.de";
      systemd.email-notify.mailFrom = "systemd <admin@frey.family>";

      services.journald.extraConfig = ''
          SystemMaxUse=1G
      '';

      system.autoUpgrade.enable = true;
      system.autoUpgrade.channel = https://nixos.org/channels/nixos-23.11;
      system.autoUpgrade.dates = "04:00";
      system.autoUpgrade.allowReboot = true;
      systemd.services.nixos-upgrade.environment.NIXOS_CONFIG = pkgs.writeText "configuration.nix" ''
        all@{ config, pkgs, lib, ... }:
              with lib; with builtins;
              let
                modifyPaths = f : paths : map toPath (map f (map toString paths));

                serverConfig = (import /etc/nixos/current/nixops.nix).server all;
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

          ln -s ${./nixops.nix} /etc/nixos/current/nixops.nix
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

      security.acme.defaults.email = "contact@pascal-wittmann.de";
      security.acme.acceptTerms = true;


      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages = with pkgs; [
        # Install only the urxvt terminfo file
        rxvt_unicode.terminfo
        zile
      ];

      # List services that you want to enable:

      # Atuin Sync Server
      services.atuin.enable = true;

      programs.msmtp = {
        enable = true;
        accounts.default = {
          auth = true;
          tls = true;
          host = "frey-family.netcup-mail.de";
          from = "admin@frey.family";
          user = "admin@frey.family";
          passwordeval = "cat /var/keys/smtp";
        };
      };

      # Cron daemon.
      services.cron.enable = true;
      services.cron.systemCronJobs = [ ];

      # Logrotate
      services.logrotate.enable = true;
      services.logrotate.settings = {
        "postgresql" = {
            files = [
              "/var/backup/postgresql/homepage_production.sql.gz"
              "/var/backup/postgresql/nextcloud.sql.gz"
            ];
            frequency = "daily";
            rotate = 30;
        };
      };

      # Enable the OpenSSH daemon
      services.openssh.enable = true;
      services.openssh.allowSFTP = true;
      services.openssh.settings =  {
        X11Forwarding = false;
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
      };

      # PostgreSQL.
      services.postgresql.enable = true;
      services.postgresql.package = pkgs.postgresql_15;
      services.postgresql.dataDir = "/var/lib/postgresql/15";
      services.postgresqlBackup.databases = [ "homepage_production" "nextcloud" ];
      services.postgresqlBackup.enable = true;
      services.postgresqlBackup.location = "/var/backup/postgresql";
      services.postgresqlBackup.startAt = "*-*-* 02:15:00";

      # MySQL
      services.mysql.enable = true;
      services.mysql.package = pkgs.mysql;

      # Caldav / Cardav
      services.radicale.enable = true;
      services.radicale.settings = {
        server = {
          hosts = [ "127.0.0.1:5232" ];
          ssl = false;
        };

        storage = {
          filesystem_folder = "/srv/radicale/collections";
          hook = ''${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m "Changes by "%(user)s && GIT_SSH_COMMAND='${pkgs.openssh}/bin/ssh -o StrictHostKeyChecking=no -i /srv/radicale/id_rsa' ${pkgs.git}/bin/git push origin)'';
        };

        auth = {
          type = "htpasswd";
          htpasswd_filename = "/var/keys/radicale";
          # encryption method used in the htpasswd file
          htpasswd_encryption = "bcrypt";
        };
      };
      services.radicale.package = pkgs.radicale3;
      services.radicale.nginx.enable = true;
      services.radicale.nginx.hostname = "calendar.pascal-wittmann.de";

      # nextcloud
      services.nextcloud.enable = true;
      services.nextcloud.package = pkgs.nextcloud27;
      services.nextcloud.home = "/srv/nextcloud";
      services.nextcloud.config.adminpassFile = "/var/keys/nextcloud";
      services.nextcloud.hostName = "cloud.pascal-wittmann.de";
      services.nextcloud.https = true;
      services.nextcloud.autoUpdateApps.enable = true;
      services.nextcloud.config = {
        dbtype = "pgsql";
        dbport = 5432;
        dbname = "nextcloud";
        dbuser = "nextcloud";
        dbpassFile = "/var/keys/databaseNextcloud";
        dbhost = "127.0.0.1";

        defaultPhoneRegion = "DE";
      };
      services.nextcloud.phpOptions = {
        "opcache.jit" = "tracing";
        "opcache.jit_buffer_size" = "100M";
        "opcache.interned_strings_buffer" = "16";
      };

      # vaultwarden
      services.vaultwarden.enable = true;
      services.vaultwarden.package = pkgs.vaultwarden;
      services.vaultwarden.backupDir = "/var/backup/vaultwarden";
      services.vaultwarden.config = {
        domain = "https://vaultwarden.pascal-wittmann.de:443";
        rocketAddress = "127.0.0.1";
        rocketPort = 8222;
        signupsAllowed = false;
      };
      services.vaultwarden.environmentFile = "/var/keys/vaultwardenEnv";
      systemd.services.vaultwarden.wants = [ "nginx.service" ];
      systemd.services.vaultwarden.after = [ "nginx.service" ];
      systemd.services.vaultwarden.bindsTo = [ "nginx.service" ];

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

        access_log /var/log/nginx/access.log anonymized;
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

        "vaultwarden.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = { proxyPass = "http://127.0.0.1:8222"; };
          extraConfig = ''
            proxy_read_timeout 90;

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

        "atuin.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = { proxyPass = "http://127.0.0.1:8888"; };
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

      # Netdata
      services.netdata.enable = true;

      # vdirsyncer
      services.vdirsyncer.enable = true;
      services.vdirsyncer.jobs.trello = {
        enable = true;
        user = "vdirsyncerTrelloUser";
        group = "vdirsyncerTrelloGroup";
        forceDiscover = true;
        timerConfig = {
          OnBootSec = "1h";
          OnUnitActiveSec = "1h";
        };
        configFile = "/var/keys/vdirsyncerTrello";
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

      virtualisation.docker.enable = true;

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

      deployment.keys.vdirsyncerTrello.text = builtins.readFile ./secrets/vdirsyncer-trello;
      deployment.keys.vdirsyncerTrello.destDir = "/var/keys";
      deployment.keys.vdirsyncerTrello.user = "vdirsyncerTrelloUser";

      deployment.keys.vaultwardenEnv.text = builtins.readFile ./secrets/vaultwarden.env;
      deployment.keys.vaultwardenEnv.destDir = "/var/keys";
      deployment.keys.vaultwardenEnv.user = "vaultwarden";
    };
}
