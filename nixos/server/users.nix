{ ... }:

{
  users.groups = {
    mail = { };
  };

  users.extraUsers = {

    pascal = {
      uid = 1000;
      group = "users";
      isNormalUser = true;
      home = "/home/pascal";
      extraGroups = [ "mpd" "mail" ];
      createHome = true;
      useDefaultShell = true;
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIC1+OlW8gz0XLwOf/Smt7KNApfoUu1Hz48eOmW9R9GnJ pascal@pascal-wittmann.de"
      ];
    };

    lerke = {
      uid = 1009;
      home = "/srv/users/lerke";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
      ];
    };

    hackaru = {
      home = "/srv/hackaru";
      isNormalUser = true;
      extraGroups = [ "docker" ];
    };

    deployer = {
      home = "/home/deployer";
      isNormalUser = true;
      group = "wheel";
      openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIObCG2o/yDrsEc/uHMAL2TDxRWvSmf1zL1zT7CG15kbE deployer@pascal-wittmann.de"
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGiS9rptVuc7SSZRFLHRUo2Hv7lN9hHQEyCFLNP9o9HC pascal@github_deploy_server"
      ];
    };
  };
}
