{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lighttpd.h5ai;
in {

  options = {
    services.lighttpd.h5ai.enable = mkEnableOption "Whether to enable h5ai";
  };

  config = mkIf cfg.enable {
    services.lighttpd.enableModules = [ "mod_fastcgi" ];
    services.lighttpd.extraConfig = ''
      index-file.names += ( "index.php", "index.html", "/_h5ai/public/index.php" )

      fastcgi.server = (
        # Load-balance requests for this path...
        ".php" => (
          # ... among the following FastCGI servers. The string naming each
          # server is just a label used in the logs to identify the server.
          "localhost" => (
            "bin-path" => "${pkgs.php}/bin/php-cgi",
            "socket" => "/tmp/php-fastcgi.sock",
            # breaks SCRIPT_FILENAME in a way that PHP can extract PATH_INFO from it
            "broken-scriptfilename" => "enable",
            # Launch (max-procs + (max-procs * PHP_FCGI_CHILDREN)) procs, where
            # max-procs are "watchers" and the rest are "workers". See:
            # https://redmine.lighttpd.net/projects/1/wiki/frequentlyaskedquestions#How-many-php-CGI-processes-will$
            "max-procs" => 4, # default value
            "bin-environment" => (
              "PHP_FCGI_CHILDREN" => "1" # default value
            )
          )
        )
      )
    '';
  };

}
