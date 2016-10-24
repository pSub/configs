{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lighttpd.subsonic;
  subsonicPort = config.services.subsonic.port;
  subsonicHost = config.services.subsonic.listenAddress;
  subsonicContextPath = config.services.subsonic.contextPath;

in {

  options = {
    services.lighttpd.subsonic.enable = mkEnableOption "Whether to enable lighttpd as reverse-proxy for subsonic";

    services.lighttpd.subsonic.hostname = mkOption {
      type = types.string;
      example = "example.com";
      description = "";
    };
  };

  config = mkIf cfg.enable {
    services.lighttpd.enableModules = [ "mod_proxy" ];
    services.lighttpd.extraConfig = ''
      $HTTP["scheme"] == "https" {
        $HTTP["host"] =~ "^(www\.|)${escape ["."] cfg.hostname}$" {
          $HTTP["url"] =~ "^${subsonicContextPath}" {
            proxy.server  = ( "" => (( "host" => "${subsonicHost}", "port" => ${toString subsonicPort} )))
          }
        }
      }
    '';
  };

}
