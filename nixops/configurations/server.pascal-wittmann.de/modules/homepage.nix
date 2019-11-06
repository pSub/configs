{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homepage;
  user = "homepage";

  homepage-app = import (pkgs.fetchFromGitHub {
    owner = "pSub";
    repo = "pascal-wittmann.de";
    rev = "572a6ffbebee3be4141db01df1cef7a4aed16459";
    sha256 = "0a62dff4chk1dn475bzx6a82ymmi00lhf7ba7ivmhvm3w8hbgadr";
    }) {};

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

  services.nginx.virtualHosts = {
     "www.pascal-wittmann.de" =  {
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
       '';
     };
  };

    systemd.services.homepage = {
      description = "Personal Homepage powered by Yesod";
      wantedBy = [ "multi-user.target" ];
      after = [ "nginx.service" "postgresql.service" ];
      bindsTo = [ "nginx.service" "postgresql.service" ];
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
