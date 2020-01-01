{ config, lib, pkgs, ... }:

with lib;

let

  sendmail = pkgs.writeScript "sendmail"
    ''
      #!/bin/sh

      ${pkgs.system-sendmail}/bin/sendmail -t <<ERRMAIL
      To: $1
      From: systemd <pascal.wittmann@gmail.com>
      Subject: Status of service $2
      Content-Transfer-Encoding: 8bit
      Content-Type: text/plain; charset=UTF-8

      $(systemctl status --full "$2")
      ERRMAIL
    '';


in
{
  options = {
    systemd.emailNotify.services = mkOption {
      type = types.listOf types.str;
      default = [];
      description = "Services for which email notifications are send in case of failure";
    };

    systemd.services = lib.mkOption {
      type = with lib.types; attrsOf (
        submodule {
          config.onFailure = [ "email@%n.service" ];
        }
      );
    };
  };

  config = {
    systemd.services."email@" = {
      description = "";
      serviceConfig = {
        ExecStart = "${sendmail} mail@pascal-wittmann.de %i";
        Type = "oneshot";
      };
    };
  };

}
