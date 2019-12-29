{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.subsonic.nginx;
  subsonicPort = config.services.subsonic.port;
  subsonicHost = config.services.subsonic.listenAddress;

in {

  options = {
    services.subsonic.nginx.enable = mkEnableOption "Whether to enable nginx as reverse-proxy for subsonic";

    services.subsonic.nginx.hostname = mkOption {
      type = types.str;
      example = "example.com";
      description = "";
    };
  };

  config = mkIf cfg.enable {
    services.nginx.virtualHosts = {
       "${cfg.hostname}" = {
         # Do not force SSL as the subsonic apps fail with "javax.net.ssl.SSLPeerUnverifiedException: No peer certificate"
         # on at least Android 4.4.
         forceSSL = false;
         addSSL = true;
         enableACME = true;
         locations."/" = { proxyPass = "http://${subsonicHost}:${toString subsonicPort}"; };
         extraConfig = ''
           add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
           add_header X-Content-Type-Options nosniff;
           add_header X-XSS-Protection "1; mode=block";
           add_header X-Frame-Options SAMEORIGIN;
         '';
       };
    };
  };

}
