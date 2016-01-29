{ config, pkgs, ... }:

{

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
}
