{ config, pkgs, lib, system, pharePkgs, ... }:

  # TODO: https://github.com/nix-community/impermanence/

let
  issueFile = pkgs.writeText "issue" ''
    Attention, by continuing to connect to this system, you consent to the owner storing a log of all activity.
    Unauthorized access is prohibited.
  '';
in  {
      require = [
        ./modules/hardware.nix
        ./modules/homepage.nix
        ./modules/radicale.nix
        ./modules/systemd-email-notify.nix
        ./users.nix
        ./mtls-secrets.nix
      ];

      nixpkgs.overlays = [
        (import ./overlays/paperless-ngx.nix) 
        (import ./overlays/pam_ssh_agent_auth.nix)
      ];

      nixpkgs.config.allowUnfree = true;

      system.stateVersion = "23.11";

      sops.defaultSopsFile = ./secrets.yaml;
      sops.defaultSopsFormat = "yaml";
      sops.age.keyFile = "/nix/secret/sops/age/keys.txt";

      sops.secrets = {
        "acme/kid" = { group = config.security.acme.defaults.group; };
        "acme/hmac" = { group = config.security.acme.defaults.group; };
        "basicauth/passwords" = { owner = "nginx"; };
        "cifs/pictures" = {};
        "changedetection" = { owner = "changedetection-io"; };
        "netdata/telegram" = { owner = "netdata"; };
        "nextcloud/admin" = { owner = "nextcloud"; };
        "nextcloud/db" = { owner = "nextcloud"; };
      # "homepage/db" = { owner = "homepage"; };
      # "paperless/admin" = { owner = "paperless"; };
        "wakapi/passwordSalt" = {  };
        "radicale" = { owner = "radicale"; };
        "restic/data" = {};
        "vaultwarden/env" = { owner = "vaultwarden"; };
        "smtp" = { group = "mail"; };
        "searx" = { owner = "uwsgi"; };
        "geoip/key" = { };
        "phare/token" = { group = "wheel"; mode = "0440"; };
        "users/root" = { };
      };

      sops.templates = {
        "changedetection.environment" = {
          content = ''
            PLAYWRIGHT_DRIVER_URL=ws://127.0.0.1:3061/?stealth=1&--disable-web-security=true&blockAds=true&--accept-lang=de-DE,de
            SALTED_PASS="${config.sops.placeholder.changedetection}"
          '';
          owner = "changedetection-io";
        };
        "acme.environment" = {
          content = ''
            LEGO_EAB=true
            LEGO_EAB_KID='${config.sops.placeholder."acme/kid"}'
            LEGO_EAB_HMAC='${config.sops.placeholder."acme/hmac"}'
          '';
          group = config.security.acme.defaults.group;
        };
      };

      # Use the systemd-boot EFI boot loader.
      boot.loader.systemd-boot.enable = true;
      boot.loader.efi.canTouchEfiVariables = true;

      # We need networking in the initrd
      boot.initrd.network = {
        enable = true;
        ssh = {
          enable = true;
          port = 10801;
          authorizedKeys = config.users.users.deployer.openssh.authorizedKeys.keys ++
                           config.users.users.github.openssh.authorizedKeys.keys ;
          hostKeys = [ "/nix/secret/initrd/ssh_host_ed25519_key" ];
        };
      };

      environment.etc."ssh/ssh_host_rsa_key" = {
        source = "/nix/persist/etc/ssh/ssh_host_rsa_key";
        mode = "0400";
      };
      environment.etc."ssh/ssh_host_rsa_key.pub" = {
        source = "/nix/persist/etc/ssh/ssh_host_rsa_key.pub";
        mode = "0400";
      };
      environment.etc."ssh/ssh_host_ed25519_key" = {
        source = "/nix/persist/etc/ssh/ssh_host_ed25519_key";
        mode = "0400";
      };
      environment.etc."ssh/ssh_host_ed25519_key.pub" = {
        source = "/nix/persist/etc/ssh/ssh_host_ed25519_key.pub";
        mode = "0400";
      };
      environment.etc."machine-id".source = "/nix/persist/etc/machine-id";

      environment.etc."ssh/ssh_backup_ed25519" = {
        source = "/nix/persist/etc/ssh/ssh_backup_ed25519";
        mode = "0400";
      };

      environment.etc."ssh/ssh_backup_ed25519.pub" = {
        source = "/nix/persist/etc/ssh/ssh_backup_ed25519.pub";
        mode = "0400";
      };

      environment.etc."issue" = {
        source = issueFile;
        mode = "0444";
      };

      environment.etc."issue.net" = {
        source = issueFile;
        mode = "0444";
      };

      boot.kernelPackages = lib.mkDefault pkgs.linuxPackages_hardened;
      boot.initrd.availableKernelModules = [
        "ata_piix"
        "uhci_hcd"
        "virtio_pci"
        "virtio_blk"
      ];
      boot.kernelModules = [ ];
      boot.extraModulePackages = [ ];

      boot.blacklistedKernelModules = [
        # Obscure network protocols
        "ax25"
        "netrom"
        "rose"

        # Old or rare or insufficiently audited filesystems
        "adfs"
        "affs"
        "bfs"
        "befs"
        "cramfs"
        "efs"
        "erofs"
        "exofs"
        "freevxfs"
        "f2fs"
        "hfs"
        "hpfs"
        "jfs"
        "minix"
        "nilfs2"
        "ntfs"
        "omfs"
        "qnx4"
        "qnx6"
        "sysv"
        "ufs"

        "tipc"
        "sctp"
        "dccp"
        "rds"

        "usb-storage"
      ];

      boot.extraModprobeConfig = ''
        install tipc /bin/true
        install sctp /bin/true
        install dccp /bin/true
        install rds  /bin/true

        install usb-storage /bin/true
      '';

      boot.kernel.sysctl = with lib; {
        "net.ipv6.conf.ens3.accept_ra_defrtr" = mkDefault false;

        "net.ipv4.conf.all.log_martians" = mkDefault true;
        "net.ipv4.conf.default.log_martians" = mkDefault true;
        "net.ipv6.conf.all.accept_redirects" = mkDefault false;
        "net.ipv6.conf.enp3s0.accept_ra" = mkDefault "0";
        "net.ipv6.conf.default.accept_redirects" = mkDefault false;
        "net.ipv4.conf.default.accept_redirects" = mkDefault false;
        "net.ipv4.conf.all.secure_redirects" = mkDefault false;
        "net.ipv4.conf.all.rp_filter" = mkDefault "1";

        # Needs to be enabled as long as I use docker.
#        "net.ipv4.conf.all.forwarding" = mkDefault "0";

        "net.ipv4.conf.all.send_redirects" = mkDefault "0";
        "net.core.bpf_jit_harden" = mkDefault "2";
        "kernel.yama.ptrace_scope" = mkOverride 500 1;
        "kernel.unprivileged_bpf_disabled" = mkDefault "1";
        "kernel.sysrq" = mkDefault "0";
        "kernel.perf_event_paranoid" = mkDefault "3";
        # This breaks the boot somehow
 #       "kernel.modules_disabled" = mkDefault "1";
        "kernel.kptr_restrict" = mkOverride 500 "2";
        "kernel.dmesg_restrict" = mkDefault "1";
        "fs.suid_dumpable" = mkDefault "0";
        "fs.protected_regular" = mkDefault "2";
        "fs.protected_fifos" = mkDefault "2";
        "dev.tty.ldisc_autoload" = mkDefault "0";
      };

      nix.settings.build-dir = "/var/tmp";
      nix.settings.allowed-users = [ ];
      nix.settings.experimental-features = [ "flakes" ];
      nix.settings.max-jobs = 2;
      nix.optimise.automatic = true;
      nix.gc = {
        automatic = true;
        dates = "weekly";
        options = "--delete-older-than 30d";
      };

      nixpkgs.config.allowBroken = true;

      # Deploy without root
      nix.settings.trusted-users = [ "root" "deployer" "github" ];
      security.sudo.enable = true;

      users.users.root.hashedPasswordFile = "/run/secrets/users/root";

      security.sudo.execWheelOnly = true;
      security.loginDefs.settings = {
        # Used values from https://github.com/dev-sec/ansible-collection-hardening/issues/365
        SHA_CRYPT_MIN_ROUNDS = 640000;
        SHA_CRYPT_MAX_ROUNDS = 640000;
      };

      security.protectKernelImage = lib.mkDefault true;

      security.auditd.enable = true;
      security.audit.enable = true;
      security.audit.rules = [
        "-a exit,always -F arch=b64 -S execve"
      ];
      environment.etc."audit/auditd.conf".text = ''
        # can following line be removed once https://github.com/CISOfy/lynis/pull/1594 is merged
        log_file = /var/log/audit/audit.log
        space_left = 10%
        space_left_action = ignore
        admin_space_left = 5%
        admin_space_left_action = email
        action_mail_acct = admin@quine.de
        num_logs = 10
        max_log_file = 100
        max_log_file_action = rotate
      '';

      services.restic.backups.server-data = {
        repository = "sftp://u465177.your-storagebox.de:23/backup";
        paths = [ "/nix/persist" ];
        exclude = [ "/srv/pictures" "/nix/persist/srv/pictures" ];
        passwordFile = "/run/secrets/restic/data";
        extraOptions = [
            "sftp.command='ssh u465177-sub2@u465177.your-storagebox.de -p 23 -i /nix/persist/etc/ssh/ssh_backup_ed25519 -o IdentitiesOnly=yes -s sftp'"
        ];
        timerConfig = {
          OnCalendar = "daily";
          Persistent = true;
        };
        pruneOpts = [
          "--keep-daily 7"
          "--keep-weekly 4"
          "--keep-monthly 3"
          "--keep-yearly 1"
        ];
        initialize = true;
      };

      systemd.email-notify.mailTo = "mail@pascal-wittmann.de";
      systemd.email-notify.mailFrom = "systemd <admin@quine.de>";

      services.journald.extraConfig = ''
          SystemMaxUse=1G
      '';

      networking.hostName = "nixos"; # Define your hostname.
      networking.domain = "pascal-wittmann.de";
      networking.nameservers = [
        "127.0.0.1"
        "9.9.9.9"
        "149.112.112.112"
      ];

      networking.firewall.enable = true;
      networking.firewall.allowPing = true;
      networking.firewall.pingLimit = "--limit 1/second --limit-burst 5";
      networking.firewall.autoLoadConntrackHelpers = false;
      networking.firewall.trustedInterfaces = [ "br-koillection" "br-solidtime" "br-dawarich"
       "br-mathesar"];
      networking.firewall.allowedTCPPorts = [
        80 # http
        443 # https
        853 # adguard
        10801 # ssh
      ];
      networking.firewall.allowedUDPPorts = [
        853 # adguard
      ];
      networking.firewall.extraCommands = ''
        iptables -I INPUT -p tcp --dport 10801 -m state --state NEW -m recent --set
        iptables -I INPUT -p tcp --dport 10801 -m state --state NEW -m recent --update --seconds 10 --hitcount 10 -j DROP
        ip6tables -I INPUT -p tcp --dport 10801 -m state --state NEW -m recent --set
        ip6tables -I INPUT -p tcp --dport 10801 -m state --state NEW -m recent --update --seconds 10 --hitcount 10 -j DROP
      '';

      # Select internationalisation properties.
      i18n.defaultLocale = "en_US.UTF-8";

      console = {
        font = "Lat2-Terminus16";
        keyMap = "de";
      };

      # Set your time zone.
      time.timeZone = "Europe/Berlin";

      # qemu guest agent
      services.qemuGuest.enable = true;

      # Security - PAM
      security.pam.sshAgentAuth.enable = true;
      security.pam.services.sudo.sshAgentAuth = true;
      security.pam.services.login.logFailures = true;
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
      security.acme.defaults.server = "https://acme-api.actalis.com/acme/directory";
      security.acme.defaults.environmentFile = "${config.sops.templates."acme.environment".path}";

      environment.systemPackages = with pkgs; [
        # Install only the alacritty terminfo file
        alacritty.terminfo

        cifs-utils
        htop
        lynis
        zile
        pgloader
      ];

      # Ensure there are no packages except the ones explicitly defined in this config
      environment.defaultPackages = lib.mkForce [];

      # AdGuard
      services.adguardhome.enable = true;
      services.adguardhome.settings = {
        auth_attempts = 3;
        block_auth_min = 10;
        http.address = "127.0.0.1:3000";

        dns = {
          protection_enabled = true;
          parental_enabled = false;
          bind_hosts = [ "0.0.0.0" ];
        };

        statistics = {
          enabled = true;
          interval = "8760h";
        };
        dhcp.enabled = false;
        tls = {
          enabled = true;
          server_name = "adguard.pascal-wittmann.de";
          port_https = 0;
          port_dns_over_tls = 853;
          certificate_path = "/run/credentials/adguardhome.service/fullchain.pem";
          private_key_path = "/run/credentials/adguardhome.service/key.pem";
        };
      };
      
      systemd.services.adguardhome.serviceConfig = {
        LoadCredential = [
          "fullchain.pem:/var/lib/acme/adguard.pascal-wittmann.de/fullchain.pem"
          "key.pem:/var/lib/acme/adguard.pascal-wittmann.de/key.pem"
        ];
      };

      # Atuin Sync Server
      services.atuin.enable = true;

      # changedetection.io
      services.changedetection-io = {
        enable = true;
        behindProxy = true;
        port = 3060;
        environmentFile = "${config.sops.templates."changedetection.environment".path}";
        baseURL = "https://changedetection.frey.family";
      };

      virtualisation = {
        oci-containers.containers = lib.mkMerge [
          { changedetection-io-playwright-ghcr = {
            image = "ghcr.io/browserless/chromium:latest";
            environment = {
              SCREEN_WIDTH = "1920";
              SCREEN_HEIGHT = "1024";
              SCREEN_DEPTH = "16";
              ENABLE_DEBUGGER = "false";
              CONCURRENT = "10";
              TIMEOUT = "60000";
              LANGUAGE = "de";
            };
            ports = [
              "127.0.0.1:3061:3000"
            ];
            extraOptions = [ "--dns=9.9.9.9" ];
          };} 
        ];
      };

      # Mail
      programs.msmtp = {
        enable = true;
        accounts.default = {
          auth = true;
          tls = true;
          host = "quine-de.netcup-mail.de";
          from = "admin@quine.de";
          user = "admin@quine.de";
          passwordeval = "cat /run/secrets/smtp";
        };
      };

      # Cron daemon.
      services.cron.enable = true;
      services.cron.systemCronJobs = [ ];

      # Logrotate
      services.logrotate.enable = true;
      # See https://discourse.nixos.org/t/logrotate-config-fails-due-to-missing-group-30000/28501
      services.logrotate.checkConfig = false;
      services.logrotate.settings = {
        "postgresql" = {
            files = lib.map (x: "/var/backup/postgresql/" + x + ".sql.gz") config.services.postgresqlBackup.databases;
            frequency = "daily";
            rotate = 30;
        };
      };

      environment.etc."logrotate.conf" = {
        source = "${config.services.logrotate.configFile}";
      };

      # fail2ban
      services.fail2ban.enable = true;
      services.fail2ban.bantime-increment.enable = true;
      services.fail2ban.jails = {
        sshd.settings = {
          enabled = true;
        };

        nginx-http-auth = ''
          enabled  = true
          port     = http,https
          logpath  = /var/log/nginx/*.log
          backend  = polling
          journalmatch =
        '';

        nginx-bad-request = ''
          enabled  = true
          port     = http,https
          logpath  = /var/log/nginx/*.log
          backend  = polling
          journalmatch =
        '';

        nginx-botsearch = ''
          enabled  = true
          port     = http,https
          logpath  = /var/log/nginx/*.log
          backend  = polling
          journalmatch =
        '';

        vaultwarden = ''
          enabled  = true
        '';

        radicale = ''
          enabled = true
        '';
      };

      environment.etc = {
        "fail2ban/filter.d/vaultwarden.conf".text = ''
             [Definition]
             failregex = ^.*Username or password is incorrect\. Try again\. IP: <ADDR>\. Username: <F-USER>.*</F-USER>\.$

             ignoreregex =
             journalmatch = _SYSTEMD_UNIT=vaultwarden.service + _COMM=vaultwarden
        '';

        "fail2ban/filter.d/radicale.conf".text = ''
             [Definition]
             failregex = ^.*Failed login attempt from .+ \(forwarded for '<ADDR>'\): '<F-USER>.+</F-USER>$
             ignoreregex =

             journalmatch = _SYSTEMD_UNIT=radicale.service + _COMM=radicale
        '';
      };

      # Enable the OpenSSH daemon
      services.openssh.enable = true;
      services.openssh.ports = [ 10801 ];
      services.openssh.allowSFTP = true;
      services.openssh.hostKeys = [
        {
          path = "/nix/secret/initrd/ssh_host_ed25519_key";
          type = "ed25519";
        }
      ];
      services.openssh.knownHosts.storageBox = {
        publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIICf9svRenC/PLKIL9nk6K/pxQgoiFC41wTNvoIncOxs";
        hostNames = [ "[u388595.your-storagebox.de]:23" "[u465177.your-storagebox.de]:23" ];
      };
      services.openssh.settings =  {
        X11Forwarding = false;
        PasswordAuthentication = false;
        KbdInteractiveAuthentication = false;
        PermitRootLogin = "no";
        MaxSessions = 2;
        MaxAuthTries = 3;
        ClientAliveCountMax = 2;
        ClientAliveInterval = 300;
        AllowTcpForwarding = "no";
        AllowAgentForwarding = "yes";
        AllowStreamLocalForwarding = "no";
        AuthenticationMethods = "publickey";
        TCPKeepAlive = "no";
      };

      # Sysstat
      services.sysstat.enable = true;

      # PostgreSQL.
      services.postgresql.enable = true;
      services.postgresql.package = pkgs.postgresql_16;
      services.postgresql.dataDir = "/var/lib/postgresql/16";
      services.postgresql.enableTCPIP = true;
      services.postgrest.settings.server-host = "127.0.0.1,172.17.0.1";
      services.postgresql.extensions = ps: with ps; [ postgis ];
      services.postgresql.authentication = lib.mkForce ''
      # TYPE  DATABASE        USER            ADDRESS                 METHOD

      # "local" is for Unix domain socket connections only
      local   all             all                                     trust
      # IPv4 local connections:
      host    all             all             127.0.0.1/32            trust
      # IPv6 local connections:
      host    all             all             ::1/128                 trust

      host    all             all             172.0.0.0/8            scram-sha-256

      # Allow replication connections from localhost, by a user with the
      # replication privilege.
      local   replication     all                                     trust
      host    replication     all             127.0.0.1/32            trust
      host    replication     all             ::1/128                 trust
      '';
      services.postgresqlBackup.databases = [ "atuin" "homepage_production" "nextcloud" "wakapi" "solidtime"
        "dawarich" "koillection" ];
      services.postgresqlBackup.enable = true;
      services.postgresqlBackup.location = "/var/backup/postgresql";
      services.postgresqlBackup.startAt = "*-*-* 02:15:00";

      # Caldav / Cardav
      services.radicale.enable = true;
      services.radicale.settings = {
        server = {
          hosts = [ "127.0.0.1:5232" ];
          ssl = false;
        };

        storage = {
          filesystem_folder = "/srv/radicale/collections";
          hook = ''${pkgs.git}/bin/git add -A && (${pkgs.git}/bin/git diff --cached --quiet || ${pkgs.git}/bin/git commit -m "Changes by "%(user)s && GIT_SSH_COMMAND='${pkgs.openssh}/bin/ssh -o StrictHostKeyChecking=no -i /srv/radicale/id_ed25519' ${pkgs.git}/bin/git push origin)'';
        };

        auth = {
          type = "htpasswd";
          htpasswd_filename = "/run/secrets/radicale";
          # encryption method used in the htpasswd file
          htpasswd_encryption = "bcrypt";
        };
      };
      services.radicale.nginx.enable = true;
      services.radicale.nginx.hostname = "calendar.pascal-wittmann.de";

      # nextcloud
      services.nextcloud.enable = true;
      services.nextcloud.package = pkgs.nextcloud31;
      services.nextcloud.home = "/srv/nextcloud";
      services.nextcloud.config.adminpassFile = "/run/secrets/nextcloud/admin";
      services.nextcloud.hostName = "cloud.pascal-wittmann.de";
      services.nextcloud.https = true;
      services.nextcloud.autoUpdateApps.enable = true;
      services.nextcloud.config = {
        dbtype = "pgsql";
        dbname = "nextcloud";
        dbuser = "nextcloud";
        dbpassFile = "/run/secrets/nextcloud/db";
        dbhost = "127.0.0.1:5432";
      };
      services.nextcloud.settings = {
        "default_phone_region" = "DE";
        "maintenance_window_start" = "3"; # UTC
      };
      services.nextcloud.phpOptions = {
        "opcache.enable" = "1";
        "opcache.enable_cli" = "1";
        "opcache.jit" = "1255";
        "opcache.jit_buffer_size" = "256M";
        "opcache.interned_strings_buffer" = "16";
        "opcache.validate_timestamps" = "0";
        "opcache.save_comments" = "1";

        "pm" = "dynamic";
        "pm.max_children" = "50";
        "pm.start_servers" = "15";
        "pm.min_spare_servers" = "15";
        "pm.max_spare_servers" = "25";
        "pm.max_requests" = "500";
      };
      services.nextcloud.phpExtraExtensions = all: [ all.redis ];

      services.redis.servers.nextcloud = {
        enable = true;
        user = "nextcloud";
        unixSocket = "/var/run/redis-nextcloud/redis.sock";
      };

      systemd.services.nextcloud-setup.serviceConfig.ExecStartPost = pkgs.writeScript "nextcloud-redis.sh" ''
          #!${pkgs.runtimeShell}
          nextcloud-occ config:system:set filelocking.enabled --value true --type bool
          nextcloud-occ config:system:set redis 'host' --value '/var/run/redis-nextcloud/redis.sock' --type string
          nextcloud-occ config:system:set redis 'port' --value 0 --type integer
          nextcloud-occ config:system:set memcache.local --value '\OC\Memcache\Redis' --type string
          nextcloud-occ config:system:set memcache.locking --value '\OC\Memcache\Redis' --type string
      '';

      # wakapi
      services.wakapi = {
        enable = true;
        passwordSaltFile = "/run/secrets/wakapi/passwordSalt";
        database.createLocally = true;
        settings = {
          server = {
            port = 3043;
          };
          db = {
            host = "127.0.0.1";
            port = 5432;
            user = "wakapi";
            name = "wakapi";
            dialect = "postgres";
          };
          security = {
            allow_signup = false;
          };
        };
      };

      # paperless
      services.paperless.enable = false;
      services.paperless.package = pkgs.paperless-ngx-without-tests;
      services.paperless.dataDir = "/srv/paperless";
      services.paperless.passwordFile = "/run/secrets/paperless/admin";
      services.paperless.settings = {
        PAPERLESS_URL = "https://paperless.pascal-wittmann.de";
        PAPERLESS_OCR_LANGUAGE = "deu+eng";
        PAPERLESS_OCR_USER_ARGS=''{"invalidate_digital_signatures": true}'';
      };

      # SearxNG
      services.searx = {
        enable = true;
        configureUwsgi = true;
        environmentFile = "/run/secrets/searx";
        settings = {
          server.port = 3070; # TODO: Has no effect, maybe because of runInUwsgi = true
          server.bind_address = "127.0.0.1";
          server.secret_key = "@SEARX_SECRET_KEY@";
          use_default_settings.engines.remove = [ "soundcloud" "wikidata "];
        };
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
      services.vaultwarden.environmentFile = "/run/secrets/vaultwarden/env";
      systemd.services.vaultwarden.wants = [ "nginx.service" ];
      systemd.services.vaultwarden.after = [ "nginx.service" ];
      systemd.services.vaultwarden.bindsTo = [ "nginx.service" ];

      # geoip update
      services.geoipupdate.enable = true;
      services.geoipupdate.settings = {
        EditionIDs = [
          "GeoLite2-ASN"
          "GeoLite2-City"
          "GeoLite2-Country"
        ];
        AccountID = 995265;
        LicenseKey = {
          _secret = "/run/secrets/geoip/key";
        };
      };

      # phare
      services.phare =  {
        enable = true;
        alertPolicyId = 16870;
        regions = [ "eu-deu-fra" ];
        interval = 180;
        tokenFile = "/run/secrets/phare/token";
        monitors = {
          ssh = {
            protocol = "tcp";
            request = {
              host = config.networking.domain;
              port = builtins.toString (builtins.elemAt config.services.openssh.ports 0);
            };
            interval = 60;
          };

          adguard = {
            protocol = "tcp";
            request = {
              host = config.networking.domain;
              port = "853";
            };
          };
        };
      };

      # nginx
      services.nginx.enable = true;
      services.nginx.recommendedBrotliSettings = true;
      services.nginx.recommendedOptimisation = true;
      services.nginx.recommendedTlsSettings = true;
      services.nginx.recommendedProxySettings = true;
      services.nginx.additionalModules = with pkgs.nginxModules; [
        geoip2
      ];

      services.nginx.commonHttpConfig = ''
       geoip2 /var/lib/GeoIP/GeoLite2-Country.mmdb {
          $geoip2_data_country_iso_code country iso_code;
        }

        map $geoip2_data_country_iso_code $is_allowed {
          default 0;
          DE 1;
          AT 1;
          GR 1;
        }

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
          enablePhare = true;
          root = "/srv/penchy";
        };

        "cloud.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          extraConfig = ''
            if ($is_allowed = 0) {
              return 403;
            }
          '';
        };

        "calendar.pascal-wittmann.de" = {
          extraConfig = ''
            if ($is_allowed = 0) {
              return 403;
            }

            proxy_set_header X-Real-IP $remote_addr;
            proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
          '';
        };

        "vaultwarden.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8222";
            proxyWebsockets = true;
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
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
          locations."/" = {
            proxyPass = "http://127.0.0.1:19999";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
          extraConfig = ''
            proxy_http_version 1.1;
            proxy_pass_request_headers on;
            proxy_set_header Connection "keep-alive";
            proxy_store off;

            ssl_verify_client on;
            ssl_client_certificate /run/secrets/mtls/netdata;

            auth_basic "Password protected area";
            auth_basic_user_file /run/secrets/basicauth/passwords;
          '';
        };

        "adguard.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3000";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
          extraConfig = ''
            ssl_verify_client on;
            ssl_client_certificate /run/secrets/mtls/adguard;
          '';
        };

        "atuin.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8888";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
           };
        };

        "actual.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:5006";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
          extraConfig = ''
            ssl_verify_client on;
            ssl_client_certificate /run/secrets/mtls/actual;
          '';
        };

        "paperless.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          extraConfig = ''
            ssl_verify_client off;
            ssl_client_certificate /run/secrets/mtls/paperless;
            client_max_body_size 0;
          '';
          locations."/" = {
            proxyPass = "http://127.0.0.1:28981";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
        };

        "wakapi.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3043";
          };
        };

        "wanderer.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3045";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
        };

        "mathesar.quine.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = false;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3048";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
        };

        "immich.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:2283";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }

              # allow large file uploads
              client_max_body_size 50000M;

              # enable websockets
              proxy_http_version 1.1;
              proxy_set_header   Upgrade    $http_upgrade;
              proxy_set_header   Connection "upgrade";
              proxy_redirect     off;

              # set timeout
              proxy_read_timeout 600s;
              proxy_send_timeout 600s;
              send_timeout       600s;
            '';
          };
        };

        "solidtime.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          phare.request.url = "https://solidtime.pascal-wittmann.de/login";
          locations."/" = {
            proxyPass = "http://127.0.0.1:3050";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
          '';
          };
        };

        "changedetection.frey.family" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3060";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
        };

        "search.quine.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:8080";
          };
        };

        "phare-nix.quine.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;
          phare.request.url = "https://phare-nix.quine.de/nixos-options/";
          root = "${pharePkgs.docs}";
        };

        "koillection.quine.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3046";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }
            '';
          };
        };

        "dawarich.quine.de" = {
          forceSSL = true;
          enableACME = true;
          locations."/" = {
            proxyPass = "http://127.0.0.1:3047";
            extraConfig = ''
              if ($is_allowed = 0) {
                return 403;
              }

              proxy_set_header Upgrade $http_upgrade;
              proxy_cache_bypass $http_upgrade;
              proxy_set_header Connection 'upgrade';
              proxy_http_version 1.1;
              proxy_redirect off;
            '';
          };
        };

        "users.pascal-wittmann.de" = {
          forceSSL = true;
          enableACME = true;
          enablePhare = true;

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
              auth_basic_user_file /run/secrets/basicauth/passwords;
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
      services.homepage.enable = false;

      # Netdata
      services.netdata.enable = true;
      services.netdata.config = {
        global = {
          "debug log" = "syslog";
          "access log" = "syslog";
          "error log" = "syslog";
        };
      };

      services.netdata.configDir = {
       "health_alarm_notify.conf" = "/run/secrets/netdata/telegram";
      };

      # Enable zsh
      programs.zsh.enable = true;

      # X-libraries and fonts are not needed on the server.
      #environment.noXlibs = true;
      fonts.fontconfig.enable = false;

      users.mutableUsers = false;
      users.defaultUserShell = "${pkgs.zsh}/bin/zsh";

      virtualisation.docker.enable = true;

      # Was enabled by changedetection.io, not sure why it seems to work without it.
      # If dns_enabled is true it leads to conflicts between aardvark and adguard.
      virtualisation.podman.defaultNetwork.settings.dns_enabled = lib.mkForce false;
}
