{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.homepage;

  homepage-app = (
    import (
      pkgs.fetchFromGitHub {
        owner = "pSub";
        repo = "pascal-wittmann.de";
        rev = "56b6e9f6f35dc1b94a8462d853e8e12f1b02c843";
        hash = "sha256-SP5NiMkFtr3TKIrZbRouidM/Lqr1POISm4ktBpRylAY=";
      }
    )
  ) { nixpkgs = pkgs; };
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
          add_header X-XSS-Protection "1; mode=block";
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
        PrivateTmp = "yes";ProtectControlGroups = true;
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
