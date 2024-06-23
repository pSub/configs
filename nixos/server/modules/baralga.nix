{ config, lib, ... }:

with lib;
let
  cfg = config.services.baralga;
in
{
  options = {
    services.baralga.enable =
      mkEnableOption "Whether to enable baralga-app";
  };

  config = mkIf cfg.enable {
    users.extraUsers.baralga = {
      uid = 493;
      home = "/var/baralga";
      group = "baralga";
      extraGroups = [ "mail" ];
    };

    users.extraGroups.baralga.name = "baralga";

    services.postgresql.ensureDatabases = [ "baralga" ];

    services.nginx.virtualHosts = {
      "baralga.pascal-wittmann.de" = {
        forceSSL = true;
        enableACME = true;
        locations."/" = { proxyPass = "http://127.0.0.1:8772"; };
        extraConfig = ''
          add_header Strict-Transport-Security "max-age=31536000; includeSubDomains" always;
          add_header X-Content-Type-Options nosniff;
          add_header X-XSS-Protection "1; mode=block";
          add_header X-Frame-Options DENY;
        '';
      };
    };

    systemd.services.baralga = {
      description = "Running baralga-app Time Tracker";
      wantedBy = [ "multi-user.target" ];
      wants = [ "nginx.service" "postgresql.service" ];
      after = [ "nginx.service" "postgresql.service" ];
      bindsTo = [ "nginx.service" "postgresql.service" ];
      environment = {
        BARALGA_DB = "postgres://baralga:VaufNafTVrofuyd2AAvB@localhost:5432/baralga";
        BARALGA_WEBROOT = "https://baralga.pascal-wittmann.de";
        PORT = "8772";
      };
      script = ''
        /var/baralga/baralga-app/baralga
      '';
      serviceConfig.KillSignal = "SIGINT";
      serviceConfig.User = "baralga";
    };
  };
}
