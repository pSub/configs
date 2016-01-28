{

  server = { pkgs, ... }:

  {

    require = [ ./modules/homepage.nix ];

    deployment.targetHost = "server.pascal-wittmann.de";

    # Use the GRUB 2 boot loader.
    boot.loader.grub.enable = true;
    boot.loader.grub.version = 2;
    # Define on which hard drive you want to install Grub.
    boot.loader.grub.device = "/dev/vda";

    boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci"
      "virtio_blk" ];
    boot.kernelModules = [ ];
    boot.extraModulePackages = [ ];

    fileSystems."/" = {
      device = "/dev/disk/by-uuid/7d067332-eba7-4a8e-acf7-a463cf50677f";
      fsType = "ext4";
    };

    swapDevices = [
      { device = "/dev/disk/by-uuid/279e433e-1ab9-4fd1-9c37-0d7e4e082944"; }
    ];

    nix.maxJobs = 2;

    networking.hostName = "nixos"; # Define your hostname.

    networking.firewall.rejectPackets = true;
    networking.firewall.allowPing = true;
    networking.firewall.autoLoadConntrackHelpers = false;
    networking.firewall.allowedTCPPorts = [
      80 # http
      443 # https
      6667 # bitlbee
      4242 # quassel
      5232 # radicale
      4567 # nixpkgs-monitor
      5000 # lint-review
      5672 # celery
    ];

    # Select internationalisation properties.
    i18n = {
      consoleFont = "Lat2-Terminus16";
      consoleKeyMap = "de";
      defaultLocale = "en_US.UTF-8";
    };

    # Set your time zone.
    time.timeZone = "Europe/Berlin";

    # Security - PAM
    security.pam.loginLimits = [ {
      domain = "*";
      item = "maxlogins";
      type = "-";
      value = "3";
    } ];

    # List packages installed in system profile. To search by name, run:
    # $ nix-env -qaP | grep wget
    environment.systemPackages = with pkgs; [
      # Install only the urxvt terminfo file
      rxvt_unicode.terminfo
      zile
    ];

  
    # List services that you want to enable:

    # Cron daemon.
    services.cron.enable = true;

    # Enable the OpenSSH daemon
    services.openssh.enable = true;
    services.openssh.allowSFTP = false;
    services.openssh.forwardX11 = false;
    services.openssh.permitRootLogin = "yes"; # For deployment via NixOps
    services.openssh.passwordAuthentication = false;
    services.openssh.challengeResponseAuthentication = false;

    # bitlbee.
    services.bitlbee.enable = true;
    services.bitlbee.interface = "0.0.0.0";
    services.bitlbee.portNumber = 6667;
    services.bitlbee.authMode = "Registered";
    services.bitlbee.hostName = "server.pascal-wittmann.de";
    services.bitlbee.configDir = "/srv/bitlbee";

    # quassel
    services.quassel.enable = true;
    services.quassel.interfaces = [ "0.0.0.0" ];
    services.quassel.dataDir = "/srv/quassel";

    # PostgreSQL.
    services.postgresql.enable = true;
    services.postgresql.package = pkgs.postgresql92;


    # Caldav / Cardav
    services.radicale.enable = true;
    services.radicale.config = ''
[server]
hosts = server.pascal-wittmann.de:5232
daemon = True
ssl = True
protocol = PROTOCOL_SSLv23
certificate = ${./secrets/cert.pem}
key = ${./secrets/key.pem}
base_prefix = /

[storage]
filesystem_folder = /srv/radicale/collections

[auth]
type = htpasswd
htpasswd_filename = ${./secrets/passwords}
htpasswd_encryption = plain
      '';

      # lighttpd.
      services.lighttpd.enable = true;
      services.lighttpd.configText = ''
        server.document-root = "/srv/www/"
        server.port = 80
        server.username = "lighttpd"
        server.follow-symlink = "enable"

        server.modules = (
          "mod_redirect",
          "mod_proxy",
          "mod_fastcgi",
          "mod_userdir",
          "mod_auth"
        )

      auth.debug = 2
      auth.backend = "plain"
      auth.backend.plain.userfile = "${./secrets/passwords}"

      userdir.basepath = "/srv/users/"
      userdir.path = ""
      userdir.include-user = ( "pascal", "qwert" )

      include "${./static/mimetype.conf}"

      static-file.exclude-extensions = ( ".fcgi", ".php", ".rb", "~", ".inc" )

      dir-listing.activate = "enable"
      dir-listing.encoding = "utf-8"

      include "/srv/homepage/lighttpd.conf"
    '';

    services.homepage.enable = true;

    # Sound
    sound.enable = false;

    # Enable zsh
    programs.zsh.enable = true;

    # X-libraries and fonts are not needed on the server.
    #  environment.noXlibs = true;
    fonts.enableFontConfig = false;

    users.mutableUsers = false;
    users.defaultUserShell = "${pkgs.zsh}/bin/zsh";
    users.extraUsers = {
      root.openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj pascal@brauchli"
      ];

      pascal = {
        uid = 1000;
        group = "users";
        home = "/home/pascal";
        extraGroups = [ "lighttpd" "mpd" ];
        createHome = true;
        useDefaultShell = true;
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCaA4eKAaVbdx7weSLzToIZ/GiysHY3Z87sg6TB1GxxfIruiPw8iS86dBvdwDYXMHsfa/3APNamqE7QC1nuH007s/wKLl8n9v4nSJPhlIh92SeFQdqKKPFMzekV7jnQ4flgGKBcqesLTHJiOyd5nh6DZ5is/FDoqnzqHYCpXfHMyCfFmWVwKt0OjowJz8kx3hrGDD8ysQ3ltqrlgcsXi3I3O/OyHK2Rhle+K2aLXfCVUePBqIp2JPBPHBPVMb0b4/gLbMXMGgJXSFUmroDsDFIDT4KqFf/5Ak3wwYxUEpQsO/jM5i7Gg2HM0x3tvONSuBMRFpkD3zBf6UtroVNs3i2z pascal@obiectiva"
          "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAwj9hapMf+bpedpEYixIbuxUMSUg0H5RngeD/rvASyfK1MOxxdOvsAJaDqJ8zvBr3DHZOdKG2BTfO2kLh1fSo9dWOLmQJEQ1t3vmd+KX1FQC5cnGR5+FrbM5npCpnSJsdMcW8qqJA2HGkrhtVklAFCkIAp5HTAT6a6KMvOIswxr3M4jJemNso1OVwt7d8pvPlSxxQmBMYCGddxQHEfDRMqvextNsgznMGdUDH05uhJ+R4qQfzV7ls5XJxOxoTdJlsuYwO9zIyGWGVeppjtESTTk9ims1Pfg6Jf9XIiokq2L4VzIoeZnpqyo9nGsUUY9M7z9QVLpMyOFqV48+hbF71+Q== 
pascal@pSub"
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj 
pascal@brauchli"
        ];
      };

      qwert = {
        uid = 1001;
        group = "users";
        home = "/home/qwert";
        createHome = true;
        useDefaultShell = true;
      };

      ragnar = {
        uid = 493;
        group = "users";
        home = "/home/ragnar";
        createHome = true;
        useDefaultShell = true;
      };
    };
  };
}
