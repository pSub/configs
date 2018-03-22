{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.homepage;
  user = "homepage";

  homepage-app = (import (pkgs.fetchFromGitHub {
    owner = "pSub";
    repo = "pascal-wittmann.de";
    rev = "1624d235fb4f89b70c6fff19bdb9e6e8b80eb117";
    sha256 = "0is2pqwqfw2lw93mdll4dbjq17gnb35plnw07mg8p9qk7qqyh4i0";
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

  services.nginx.virtualHosts = {
     "(www.)?pascal-wittmann.de" = {
       forceSSL = true;
       sslCertificate = "/srv/homepage/ssl/nginx/ssl-bundle.crt";
       sslCertificateKey = "/srv/homepage/ssl/nginx/pascal-wittmann.de.key";
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
