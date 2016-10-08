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

    services.lighttpd.enableModules = [ "mod_redirect" "mod_proxy" "mod_setenv" ];
    services.lighttpd.extraConfig = ''
      name = "pascal-wittmann.de"
      protocol = "https"
      approute = protocol + "://" + name + "/"

      $HTTP["scheme"] == "http" {
        $HTTP["host"] =~ "^(www\.|)pascal-wittmann\.de$" {
          url.redirect = ( "^/(.*)" => approute + "$1")
        }
      }

      $HTTP["scheme"] == "https" {
        $HTTP["host"] =~ "^(www\.|)pascal-wittmann\.de$" {
          ssl.engine                  = "enable"
          ssl.pemfile                 = "/srv/homepage/ssl/www.pascal-wittmann.de.pem"
          ssl.ca-file                 = "/srv/homepage/ssl/ca.crt"
          ssl.cipher-list = "ECDHE-RSA-AES128-GCM-SHA256:ECDHE-ECDSA-AES128-GCM-SHA256:ECDHE-RSA-AES256-GCM-SHA384:ECDHE-ECDSA-AES256-GCM-SHA384:DHE-RSA-AES128-GCM-SHA256:DHE-DSS-AES128-GCM-SHA256:kEDH+AESGCM:ECDHE-RSA-AES128-SHA256:ECDHE-ECDSA-AES128-SHA256:ECDHE-RSA-AES128-SHA:ECDHE-ECDSA-AES128-SHA:ECDHE-RSA-AES256-SHA384:ECDHE-ECDSA-AES256-SHA384:ECDHE-RSA-AES256-SHA:ECDHE-ECDSA-AES256-SHA:DHE-RSA-AES128-SHA256:DHE-RSA-AES128-SHA:DHE-DSS-AES128-SHA256:DHE-RSA-AES256-SHA256:DHE-DSS-AES256-SHA:DHE-RSA-AES256-SHA:AES128-GCM-SHA256:AES256-GCM-SHA384:AES128-SHA256:AES256-SHA256:AES128-SHA:AES256-SHA:AES:CAMELLIA:DES-CBC3-SHA:!aNULL:!eNULL:!EXPORT:!DES:!RC4:!MD5:!PSK:!aECDH:!EDH-DSS-DES-CBC3-SHA:!EDH-RSA-DES-CBC3-SHA:!KRB5-DES-CBC3-SHA "
          ssl.dh-file="/srv/homepage/ssl/dhparams.pem"

          setenv.add-response-header = ( "Strict-Transport-Security" => "max-age=63072000; includeSubDomains; preload" )

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
