{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.homepage;
  user = "homepage";

  homepage-app = (
    import (
      pkgs.fetchFromGitHub {
        owner = "pSub";
        repo = "pascal-wittmann.de";
        rev = "ec1d2befec1a9670ae1b6452ec8b6ead80e348fb";
        sha256 = "0l04x7xxhn062v57gv8k1izl35kpsh24d9hx42r5jlvrcilwlxfh";
      }
    )
  ) {
    nixpkgs = import (
      fetchTarball
        "https://github.com/NixOS/nixpkgs-channels/archive/nixos-19.09-small.tar.gz"
    )
      {};
  };
in
{
  options = {
    services.homepage.enable =
      mkEnableOption "Whether to enable pascal-wittmann.de";
  };

  config = mkIf cfg.enable {
    users.extraUsers = singleton {
      name = user;
      uid = 492;
      description = "Homepage pascal-wittmann.de";
      home = "/var/homepage";
      extraGroups = [ "mail" ];
    };

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
        locations."/" = { proxyPass = "http://127.0.0.1:3001"; };
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
          add_header X-Content-Type-Options nosniff;
          add_header X-XSS-Protection "1; mode=block";
          add_header X-Frame-Options DENY;

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
        PGUSER = user;
        PGDATABASE = "homepage_production";
      };
      script = ''
        export PGPASS=`cat /var/keys/databaseHomepage`
        cd /srv/homepage
        ${homepage-app}/bin/homepage
      '';
      serviceConfig.KillSignal = "SIGINT";
      serviceConfig.User = user;
    };
  };
}
