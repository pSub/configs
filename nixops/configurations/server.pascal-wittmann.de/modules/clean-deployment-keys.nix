{ config, lib, pkgs, ... }:

{
  config = {
    system.activationScripts.cleanKeys = ''
      ${pkgs.findutils}/bin/find /var/keys -type f \
        ${lib.concatMapStringsSep " " (key: "-not -name '${key}'") (builtins.attrNames config.deployment.keys)} \
        -printf 'delete obsolete key %p\n' -delete
    '';
  };
}
