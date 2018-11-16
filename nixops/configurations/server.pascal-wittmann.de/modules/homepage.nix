{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homepage;
  user = "homepage";

  homepage-app = (import (pkgs.fetchFromGitHub {
    owner = "pSub";
    repo = "pascal-wittmann.de";
    rev = "a1c2fc8b71ecd5aa23342be606a8415ffe8d87bc";
    sha256 = "0zbh4zjvz5qaf6l4lh4lh0c0k9gm5yv4ph2z25pkhif5yf7zyd0k";
  })) { nixpkgs = import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/nixos-18.03-small.tar.gz) {}; };

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
       globalRedirect = "pascal-wittmann.de";
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
      after = [ "lighttpd.service" "postgresql.service" ];
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
