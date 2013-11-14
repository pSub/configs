# the system.  Help is available in the configuration.nix(5) man page
# or the NixOS manual available on virtual console 8 (Alt+F8) 

{ config, pkgs, ... }:

let hydra = pkgs.fetchgit { url = https://github.com/NixOS/hydra; rev = "refs/heads/master"; };

in {
  require =
    [ # Include the results of the hardware scan.
      ./hardware-configuration.nix
      "${hydra}/hydra-module.nix"
    ];

  boot.kernelPackages = pkgs.linuxPackages_3_10;

  boot.initrd.kernelModules =
    [ # Specify all kernel modules that are necessary for mounting the root
      # filesystem.
      # "xfs" "ata_piix"
    ];
    
  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.timeout = 2;

  # Define on which hard drive you want to install Grub.
  boot.loader.grub.device = "/dev/vda";

  networking.hostName = "obiectiva";
  networking.wireless.enable = false;

  # Firewall settings.
  networking.firewall.enable = true;
  networking.firewall.rejectPackets = true;
  networking.firewall.allowPing = true;
  networking.firewall.autoLoadConntrackHelpers = false;
  networking.firewall.allowedTCPPorts = [
    80 # http
    443 # https
#    3000 # hydra
    6667 # bitlbee
    51413 # torrent
  ];

  # Add filesystem entries for each partition that you want to see
  # mounted at boot time.  This should include at least the root
  # filesystem.

  fileSystems."/".device = "/dev/mapper/volume-root";
  fileSystems."/boot".device = "/dev/vda1";

  # List swap partitions activated at boot time.
  swapDevices =
    [ { device = "/dev/mapper/volume-swap_1"; }
    ];

  # Select internationalisation properties.
  i18n = {
    consoleFont = "lat9w-16";
    consoleKeyMap = "de";
    defaultLocale = "en_US.UTF-8";
  };

  # Use zsh globally.
  programs.bash.enable = false;
  programs.zsh.enable = true;

  # Cron daemon.
  services.cron.enable = true;

  # Logfile scanner settings.
  services.logcheck.enable = true;

  # Hydra
  #services.hydra.enable = false;
  #services.hydra.hydra = (import "${hydra}/release.nix" {}).build.x86_64-linux;
  #services.hydra.hydraURL = "http://psub.eu";
  #services.hydra.port = 3000;
  #services.hydra.notificationSender = "hydra@psub.eu";

  # The OpenSSH daemon.
  services.openssh.enable = true;
  services.openssh.allowSFTP = false;
  services.openssh.forwardX11 = false;
  services.openssh.permitRootLogin = "no";
  services.openssh.passwordAuthentication = false;
  services.openssh.challengeResponseAuthentication = false;

  # OpenVPN.
  services.openvpn.enable = false;

  # bitlbee.
  services.bitlbee.enable = true;
  services.bitlbee.interface = "0.0.0.0";
  services.bitlbee.portNumber = 6667;
  services.bitlbee.authMode = "Registered";
  services.bitlbee.extraSettings = "HostName = im.psub.eu";

  # lighttpd.
  services.lighttpd.enable = true;
  services.lighttpd.configText = ''
    server.document-root = "/srv/www/"
    server.port = 80
    server.username = "lighttpd"
    server.group = "lighttpd"
    server.modules = (
      "mod_redirect",
      "mod_proxy",
      "mod_fastcgi",
      "mod_userdir"
    )

    userdir.basepath = "/srv/users/"
    userdir.path = ""
    userdir.include-user = ( "pascal" )

    index-file.names = ( "index.html", "index.php" )

    include "/srv/mimetype.conf"

    static-file.exclude-extensions = ( ".fcgi", ".php", ".rb", "~", ".inc" )

    dir-listing.activate = "enable"
    dir-listing.encoding = "utf-8"

    include "/home/pascal/pw/lighttpd.conf"

    $HTTP["host"] == "hainzenklingen.psub.eu" {
      server.document-root = "/srv/hainzenklingen/"
      server.error-handler-404 = "/index.php"
      fastcgi.server    = ( ".php" => ((
        "bin-path" => "${pkgs.php}/bin/php-cgi -c /srv/php.ini",
        "socket" => "/tmp/php.socket",
        "bin-environment" => ( 
          "PHP_FCGI_CHILDREN" => "16",
          "PHP_FCGI_MAX_REQUESTS" => "10000"
         ),
        "bin-copy-environment" => (
          "PATH", "SHELL", "USER" )
      )))
    }
  '';

  # Systemd service for my homepage
  systemd.services.homepage = {
      description = "Personal Homepage powered by Yesod";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      requires = [ "lighttpd.service" ];
      script = ''
        cd /home/pascal/pw/
        /home/pascal/pw/homepage Production
      '';
      serviceConfig.KillSignal = "SIGINT";
    };

  # PostgreSQL.
  services.postgresql.enable = true;
  services.postgresql.package = pkgs.postgresql92; 

  # MySQL
  services.mysql.enable = true;

  # Torrent
  services.transmission.enable = true;
  services.transmission.settings = 
    { download-dir = "/srv/torrents";
      incomplete-dir = "/srv/torrents/.incomplete";
      incomplete-dir-enabled = true;
      speed-limit-up = 1000;
      speed-limit-up-enabled = true;

      # for users in group "transmission" to have access to torrents
      umask = 2;
    };

  # Tor.
  #services.tor.relay.enable = true;
  #services.tor.relay.isExit = false;
  #services.tor.relay.bandwidthRate = 100;
  #services.tor.relay.bandwidthBurst = 200;

  # Sound
  sound.enable = false;

  # Timezone
  time.timeZone = "Europe/Berlin";

  # system software.
  environment.systemPackages = with pkgs; [
	file
	htop
	unison
        msmtp
	vim
	zsh
  ];

  # X-libraries and fonts are not needed on the server.
  environment.noXlibs = true;
  fonts.enableFontConfig = false;

  users.extraUsers = {
    pascal = {
      group = "users";
      home = "/home/pascal";
      extraGroups = [ "lighttpd" ];
      createHome = true;
      shell = "/var/run/current-system/sw/bin/zsh";
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDK3zOLu2oy3hw1GN2N5BjXrhz+/FEF/KUCYHWuSh2f3FqE+6Nyae+J8mp2z5PPzVpwO3M5l/k5mCcPLODv7EnnNg8dlq2/TwecFp6VfwE4MW4VWldd6Zlqs/V9w8Pk4YvwDRZPZD9+ldTxx6KSm00hXBJ1RHyZBSemRkTrFyVmR/iR03A+NakztVIAe/uklIyos/JL4g6u7pQ8LcveHlcYsqPd6Oa2tHDiprvmXBmzJjxOaZVllynX1T8UOrq4fZFMbx077zD6Z6+TE9EKYQ7B1RbtyDUzab9UKi4MLerCf3PRavPQgqKGkqFr0lW06ssvZvw9H4+hnvjhSaMRGMy3 pascal@obiectiva"
        "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAwj9hapMf+bpedpEYixIbuxUMSUg0H5RngeD/rvASyfK1MOxxdOvsAJaDqJ8zvBr3DHZOdKG2BTfO2kLh1fSo9dWOLmQJEQ1t3vmd+KX1FQC5cnGR5+FrbM5npCpnSJsdMcW8qqJA2HGkrhtVklAFCkIAp5HTAT6a6KMvOIswxr3M4jJemNso1OVwt7d8pvPlSxxQmBMYCGddxQHEfDRMqvextNsgznMGdUDH05uhJ+R4qQfzV7ls5XJxOxoTdJlsuYwO9zIyGWGVeppjtESTTk9ims1Pfg6Jf9XIiokq2L4VzIoeZnpqyo9nGsUUY9M7z9QVLpMyOFqV48+hbF71+Q== pascal@pSub"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj pascal@brauchli"
      ];
    };
    qwert = {
       group = "users";
       home = "/home/qwert";
       createHome = true;
       shell = "/var/run/current-system/sw/bin/zsh";
    };
  };
  
}
