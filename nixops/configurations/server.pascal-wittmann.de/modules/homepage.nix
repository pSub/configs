{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homepage;
  revision = "ab45143f7c3d47e0f0fcc0aca3a62baebaa242f2";
  user = "homepage";

  homepage-app = (import (pkgs.fetchFromGitHub {
    owner = "pSub";
    repo = "pascal-wittmann.de";
    rev = "ab45143f7c3d47e0f0fcc0aca3a62baebaa242f2";
    sha256 = "12l4g3k29f6z6v6l7glb7dpbfzsf14rvnw73agf8gsali2bdqpnd";
  })) { };

in {
  options = {
    services.homepage.enable = mkEnableOption "Wheter to enable pascal-wittmann.de";
  };

  config = mkIf cfg.enable {
    users.extraUsers = singleton
    { name = user;
      uid = 492;
      description = "Homepage pascal-wittmann.de";
      home = "/var/homepage";
    };

    services.lighttpd.enableModules = [ "mod_redirect" "mod_proxy" "mod_setenv" ];
    services.lighttpd.extraConfig = ''
      $HTTP["scheme"] == "http" {
        $HTTP["host"] =~ "^(www\.|)pascal-wittmann\.de$" {
          url.redirect = (".*" => "https://%0$0")
        }
      }

      $HTTP["scheme"] == "https" {
        $HTTP["host"] =~ "^(www\.|)pascal-wittmann\.de$" {
          ssl.engine                  = "enable"
          ssl.pemfile                 = "/srv/homepage/ssl/www.pascal-wittmann.de.pem"
          ssl.ca-file                 = "/srv/homepage/ssl/ca.crt"
          ssl.cipher-list = "${import ../static/ssl-cipher-list.txt}"
          ssl.dh-file = "/srv/homepage/ssl/dhparams.pem"

          setenv.add-response-header = (
            "Strict-Transport-Security" => "max-age=63072000; includeSubDomains; preload",
            "X-Content-Type-Options" => "nosniff",
            "X-Frame-Options" => "DENY",
            "X-XSS-Protection" => "1; mode=block",
            "Public-Key-Pins" => "pin-sha256=\"aiHvkTqXNmsZ9V78XaIbP6VHV5O2Q1oN85+N/r3qATA=\"; pin-sha256=\"ZjOx5W+YxpIcqzuFaFr4o0yXxxu1QrUhIq5NFpdy9zY=\"; max-age=5184000; includeSubdomains"
          )

          proxy.balance = "hash"
          proxy.server  = ( "" => (( "host" => "127.0.0.1", "port" => 3001 )))
        }
      }
    '';

    systemd.services.homepage = {
      description = "Personal Homepage powered by Yesod";
      wantedBy = [ "multi-user.target" ];
      after = [ "lighttpd.service" "postgresql.service" ];
      bindsTo = [ "lighttpd.service" "postgresql.service" ];
      environment = {
        APPROOT = "https://www.pascal-wittmann.de";
        PORT = "3001";
        PGUSER = user;
        PGPASS = import ../secrets/homepage_database_password;
        PGDATABASE = "homepage_production";
        GITHUB_OAUTH_CLIENT_ID = "82fa60e9329799fe88f8";
        GITHUB_OAUTH_CLIENT_SECRET = import ../secrets/github_oauth_client_secret;
      };
      script = ''
        cd /srv/homepage
        ${homepage-app}/bin/homepage
      '';
      serviceConfig.KillSignal = "SIGINT";
      serviceConfig.User = "homepage";
    };
  };
}
