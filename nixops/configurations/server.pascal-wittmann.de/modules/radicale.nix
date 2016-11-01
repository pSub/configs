{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lighttpd.radicale;

in {

  options = {
    services.lighttpd.radicale.enable = mkEnableOption "Whether to enable lighttpd as reverse-proxy for radicale";

    services.lighttpd.radicale.hostname = mkOption {
      type = types.string;
      example = "example.com";
      description = "";
    };

  };

  config = mkIf cfg.enable {
    services.lighttpd.enableModules = [ "mod_proxy" "mod_auth" ];
    services.lighttpd.extraConfig = ''
      $HTTP["scheme"] == "https" {
        $HTTP["host"] =~ "^(www\.|)${escape ["."] cfg.hostname}$" {
          $HTTP["url"] =~ "^/radicale/" {
            proxy.server  = ( "" => (( "host" => "127.0.0.1", "port" => 5232 )))
            auth.require = (
              "/radicale" => (
                "method" => "basic",
                "realm"     => "Password protected area",
                "require"   => "valid-user"
              )
            )
          }
        }
      }
    '';
  };

}
