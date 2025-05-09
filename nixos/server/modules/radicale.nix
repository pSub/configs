{ config, lib, ... }:

with lib;

let
  cfg = config.services.radicale.nginx;

in
{

  options = {
    services.radicale.nginx.enable = mkEnableOption "Whether to enable nginx as reverse-proxy for radicale";

    services.radicale.nginx.hostname = mkOption {
      type = types.str;
      example = "example.com";
      description = "";
    };

  };

  config = mkIf cfg.enable {
    services.nginx.virtualHosts = {
      "${cfg.hostname}" = {
        forceSSL = true;
        enableACME = true;
        enablePhare = true;
        phare.request.url = "https://calendar.pascal-wittmann.de/.web/";
        locations."/" = { proxyPass = "http://127.0.0.1:5232"; };
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
          add_header X-Content-Type-Options nosniff;
          add_header X-XSS-Protection "1; mode=block";
          add_header X-Frame-Options DENY;
        '';
      };
    };
  };

}
