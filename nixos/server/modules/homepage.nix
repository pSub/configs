{ config, lib, pkgs, pkgs-old, ... }:

with lib;
let
  cfg = config.services.homepage;

  homepage-app = (
    import (
      pkgs.fetchFromGitHub {
        owner = "pSub";
        repo = "pascal-wittmann.de";
        rev = "e45bbc3581e97119928085cd0fdb704dcf01b042";
        hash = "sha256-KTM9cFCV0r3n1yFSJANvRayzF6PAv0dDuSh74QdX+AA==";
      }
    )
  ) { nixpkgs =  import (fetchTarball {
    url = "https://channels.nixos.org/nixos-25.05/nixexprs.tar.xz";
    sha256 = "0151xw5fjmdcw761rs5lh1237l6galx0j78bl9557rciwb653sbs";
  }){};
   };
in
{
  options = {
    services.homepage.enable =
      mkEnableOption "Whether to enable pascal-wittmann.de";
  };

  config = mkIf cfg.enable {
    users.extraUsers.homepage = {
      uid = 492;
      description = "Homepage pascal-wittmann.de";
      home = "/var/homepage";
      group = "homepage";
      extraGroups = [ "mail" ];
    };

    users.extraGroups.homepage.name = "homepage";

    services.postgresql.ensureDatabases = [ "homepage_production" ];

    services.nginx.virtualHosts = {
      "www.pascal-wittmann.de" = {
        forceSSL = true;
        enableACME = true;

        locations."/" = {
          extraConfig = "return 301 https://pascal-wittmann.de$request_uri;";
        };

      };

      "pascal-wittmann.de" = {
        forceSSL = true;
        enableACME = true;
        enablePhare = true;
        locations."/" = { proxyPass = "http://127.0.0.1:3001"; };
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
          add_header X-Content-Type-Options nosniff;
          add_header Access-Control-Allow-Origin "https://pascal-wittmann.de";
          add_header X-Frame-Options DENY;
          add_header Permissions-Policy "camera=(), microphone=(), geolocation=(), fullscreen=()";
          add_header Referrer-Policy "strict-origin-when-cross-origin" always;

          rewrite ^/datenschutz$ /privacy permanent;
          rewrite ^/impressum$ /contact permanent;
          rewrite ^/category/(.+)/entry/(.+)$ /entry/$2 permanent;
          rewrite ^/category/(.+)/entry/(.+)/reply-to/(\d+)$ /entry/$2/reply-to/$3 permanent;
        '';
      };
    };

    systemd.services.homepage = {
      description = "Personal Homepage powered by Yesod";
      wantedBy = [ "multi-user.target" ];
      wants = [ "nginx.service" "postgresql.service" ];
      after = [ "nginx.service" "postgresql.service" ];
      bindsTo = [ "nginx.service" "postgresql.service" ];
      environment = {
        APPROOT = "https://pascal-wittmann.de";
        PORT = "3001";
        PGUSER = "homepage";
        PGDATABASE = "homepage_production";
      };
      script = ''
        export PGPASS=`cat /run/secrets/homepage/db`
        cd /srv/homepage
        ${homepage-app}/bin/homepage
      '';
      serviceConfig = {
        KillSignal = "SIGINT";
        User = "homepage";
        KeyringMode = "private";
        LockPersonality = true;
        MemoryDenyWriteExecute = true;
        NoNewPrivileges = true;
        PrivateMounts = "yes";
        PrivateTmp = "yes";
        ProtectControlGroups = true;
        ProtectHome = "yes";
        ProtectHostname = true;
        ProtectKernelModules = true;
        ProtectKernelTunables = true;
        ProtectSystem = "strict";
        ReadWritePaths = [ "/srv/homepage" ];
        RemoveIPC = true;
        RestrictAddressFamilies = [ "AF_UNIX" "AF_INET" "AF_INET6" ];
        RestrictNamespaces = true;
        RestrictRealtime = true;
        RestrictSUIDSGID = true;
        SystemCallFilter = "@system-service @clock";
        SystemCallArchitectures = "native";
      };
    };
  };
}
