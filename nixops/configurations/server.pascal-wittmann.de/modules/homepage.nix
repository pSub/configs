{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homepage;
  user = "homepage";

  homepage-app = (import (pkgs.fetchFromGitHub {
    owner = "pSub";
    repo = "pascal-wittmann.de";
    rev = "fb485ca2ec81f40a5cae146e7c3d42478f54fe48";
    sha256 = "1sf0nm6k0i7xjngax19akzq7m4y0g8p4yy7z29jh6303yp1kl6q3";
  })) { nixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-17.09-small.tar.gz) {}; };

in {
  options = {
    services.homepage.enable = mkEnableOption "Whether to enable pascal-wittmann.de";
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
          $HTTP["url"] !~ "^/(~|_h5ai|music|nixpkgs-monitor|radicale)(.*)$" {
            proxy.balance = "hash"
            proxy.server  = ( "" => (( "host" => "127.0.0.1", "port" => 3001 )))
          }
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
