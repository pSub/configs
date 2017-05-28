{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lighttpd.nixpkgs-monitor;
in {

  options = {
    services.lighttpd.nixpkgs-monitor.enable = mkEnableOption "Whether to enable lighttpd as reverse-proxy for nixpkgs-monitor";

    services.lighttpd.nixpkgs-monitor.hostname = mkOption {
      type = types.string;
      example = "example.com";
      description = "";
    };
  };

  config = mkIf cfg.enable {
    services.lighttpd.enableModules = [ "mod_proxy" "mod_magnet" ];
    services.lighttpd.extraConfig = ''
      $HTTP["scheme"] == "https" {
        $HTTP["host"] =~ "^(www\.|)${escape ["."] cfg.hostname}$" {
          $HTTP["url"] =~ "^/nixpkgs-monitor" {
            magnet.attract-raw-url-to = ( "${pkgs.writeTextFile {
              name = "lua-rewrite-script";
              text = ''
                lighty.env["request.uri"] = string.sub(lighty.env["request.uri"], string.len('/nixpkgs-monitor/'))
                return
              '';
            }}" )
            proxy.server  = ( "" => ( "nixpkgs-monitor" => ( "host" => "127.0.0.1", "port" => 4567 )))
          }
        }
      }
    '';

    nixpkgs.config = {
      packageOverrides = super: let self = super.pkgs; in {
        lighttpd = super.lighttpd.override { enableMagnet = true; };
      };
    };
  };

}
