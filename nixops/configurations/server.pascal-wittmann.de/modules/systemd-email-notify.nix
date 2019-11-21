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
  
in {

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
