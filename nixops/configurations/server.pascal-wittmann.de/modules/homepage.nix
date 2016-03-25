{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homepage;

  user = "homepage";
  homepage-app = (import /home/pascal/web/pascal-wittmann.de) { };
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

    services.lighttpd.enableModules = [ "mod_redirect" "mod_proxy" ];
    services.lighttpd.extraConfig = ''
      name = "www.pascal-wittmann.de"
      protocol = "http"
      approute = protocol + "://" + name + "/"
      
      $HTTP["host"] == "pascal-wittmann.de" {
        url.redirect = ( "^/(.*)" => approute + "$1")
      }

      $HTTP["host"] == "www.pascal-wittmann.de" {
        $SERVER["socket"] == ":443" {
          ssl.engine                  = "enable"
          ssl.pemfile                 = "/srv/homepage/ssl/www.pascal-wittmann.de.pem" 
          ssl.ca-file                 = "/srv/homepage/ssl/ca.crt"
        }
        proxy.balance = "hash"
        proxy.server  = ( "" => (( "host" => "127.0.0.1", "port" => 3001 )))
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
