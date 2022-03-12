{ ... }:

{
  users.groups = {
    mail = {};
  };

  users.extraUsers = {
    root.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj pascal@brauchli"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFMsKQmC81HP+WrkINeslvRe7BWkx0a2e5F7BRDLTXGI NixOps client key for server"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCz/EsycVbiWMIhIOjJICo5uiCI7glX50C3y8cszY372o6VXY/hsT6RLM07Ch2CbhumUJnjJrxFG/GhiI3Hes16BrAHyGlWu3jLowJXSqWa3Z5X2vWNrQXrR5RH66vFiaWCgbZX9mH9rcFW49UYSssuHgBNzO2O/a4nwT8z7qeB4MGZr684PKAmxTwRFp9NTDnQsKMlGY0kTGfbBZ5Ms/HMeDDsCqkitfYl4Sv29ku6N3OMUgCbsWLn4p0PC7KJq540B15n3ct2UVi2J/rzSNt213NzL5pfRgzK18nHqXNVdVY2yuWAfxEfOiy5B+nKunrRZ9CcS4zK8y3vWp8BZJk4xzKIqp7vrS2h7AS58kOMwQA6gxTgACmsmqTacCzjci1aKu96cKxy2Z1ucJOsG/SMva2OydnR0VDnrgsAUKFdTAfzya508RJ0nhWWnhrQqUqzWYpwuQQY3Yi9bOnDMn2MNWcmAnxVOV7fY/btqgW1Z/NIfvD9gqg29Q0HMgAzxaHVNb0vJf9Xlq9JQFdBjTLLUwF4hrIxa0fyUdsNLSPajt7XKN61CeYFk5JhbXvh1NWIN8HEVXjY9Y1il/nyT+G7NJvyDAA6jJAqd4gB6yl0HF9zhtL5fLEpwSObNki5p+J0oA5gRkAzWHTSH0ceiNuKk5e6arnPAXzCDbaYAN8J/Q== red6"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIDc9ad0nrS23JRQeEwSvGp106YnbOQ0Rfjcmh7AX5iij Pascal Wittmann <pascal.wittmann@red6-es.de>"
    ];

    pascal = {
      uid = 1000;
      group = "users";
      isNormalUser = true;
      home = "/home/pascal";
      extraGroups = [ "mpd" "mail" ];
      createHome = true;
      useDefaultShell = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCaA4eKAaVbdx7weSLzToIZ/GiysHY3Z87sg6TB1GxxfIruiPw8iS86dBvdwDYXMHsfa/3APNamqE7QC1nuH007s/wKLl8n9v4nSJPhlIh92SeFQdqKKPFMzekV7jnQ4flgGKBcqesLTHJiOyd5nh6DZ5is/FDoqnzqHYCpXfHMyCfFmWVwKt0OjowJz8kx3hrGDD8ysQ3ltqrlgcsXi3I3O/OyHK2Rhle+K2aLXfCVUePBqIp2JPBPHBPVMb0b4/gLbMXMGgJXSFUmroDsDFIDT4KqFf/5Ak3wwYxUEpQsO/jM5i7Gg2HM0x3tvONSuBMRFpkD3zBf6UtroVNs3i2z pascal@obiectiva"
        "ssh-rsa AAAAB3NzaC1yc2EAAAABIwAAAQEAwj9hapMf+bpedpEYixIbuxUMSUg0H5RngeD/rvASyfK1MOxxdOvsAJaDqJ8zvBr3DHZOdKG2BTfO2kLh1fSo9dWOLmQJEQ1t3vmd+KX1FQC5cnGR5+FrbM5npCpnSJsdMcW8qqJA2HGkrhtVklAFCkIAp5HTAT6a6KMvOIswxr3M4jJemNso1OVwt7d8pvPlSxxQmBMYCGddxQHEfDRMqvextNsgznMGdUDH05uhJ+R4qQfzV7ls5XJxOxoTdJlsuYwO9zIyGWGVeppjtESTTk9ims1Pfg6Jf9XIiokq2L4VzIoeZnpqyo9nGsUUY9M7z9QVLpMyOFqV48+hbF71+Q== pascal@pSub"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj pascal@brauchli"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQClsvb39kUwxmETz5wNRnbRtWKFxunxB7rsKTHqMfVtXq6kwEUrM3epga7Wh7LV+z90OOvvSEYIl4q1vSIyVhtTU1iGX2oWqrZyzD27USyW0+/mxhE7ebV18f9kMeO1rXZ9hR6JXR3DKIQjHFq275sGfx/PMMP6KbpKc30KVv1VfLU87vBLi6RGLrrD9SRTQiyvX3H7OIS2F+eVgOJIE+ut/Ic3xtgx2MJ5oXnl6yxnxJV2R5K4K2Ykx/t+U0027YHwnsh0kKhUigdgMo7ZHaJ6RcijLVUPrV9C/kYIRwW/B9LHNKSoeztpG3/8z/1eAQKxn0bjJzePUdMLSl/JmmQdcVeHWFckp9hUsHjRnGYNOc5AWhJ0WHLW4rHwJTBgo9JZZ58hnu+rAxg3FW5kRI4+46pDCnDnmSUao//f94djNmqUIAuDKIjivYLqfPhDYeBXwfCsIXKqXpyhFFTFTaMlspvfXi5A+pAfN68nn8p4kE41aiFtruDUiP/SUOC/ytl0K+riTWIS252gSKXlu0+52u6WkjTUWaCoCufl7q7XrWytZYZrrivIrLeg3iNl1YNoYf0tHrA++VwpwTmcG5Z+83Ygq07bE1Fc66LT8ElSmTCB15HlxryT9ThiAlXkVP7GIvbGJYzOq8PiAq0WbL/vSg9HYUgMCvybqMK674IjIQ== kyocera"
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCz/EsycVbiWMIhIOjJICo5uiCI7glX50C3y8cszY372o6VXY/hsT6RLM07Ch2CbhumUJnjJrxFG/GhiI3Hes16BrAHyGlWu3jLowJXSqWa3Z5X2vWNrQXrR5RH66vFiaWCgbZX9mH9rcFW49UYSssuHgBNzO2O/a4nwT8z7qeB4MGZr684PKAmxTwRFp9NTDnQsKMlGY0kTGfbBZ5Ms/HMeDDsCqkitfYl4Sv29ku6N3OMUgCbsWLn4p0PC7KJq540B15n3ct2UVi2J/rzSNt213NzL5pfRgzK18nHqXNVdVY2yuWAfxEfOiy5B+nKunrRZ9CcS4zK8y3vWp8BZJk4xzKIqp7vrS2h7AS58kOMwQA6gxTgACmsmqTacCzjci1aKu96cKxy2Z1ucJOsG/SMva2OydnR0VDnrgsAUKFdTAfzya508RJ0nhWWnhrQqUqzWYpwuQQY3Yi9bOnDMn2MNWcmAnxVOV7fY/btqgW1Z/NIfvD9gqg29Q0HMgAzxaHVNb0vJf9Xlq9JQFdBjTLLUwF4hrIxa0fyUdsNLSPajt7XKN61CeYFk5JhbXvh1NWIN8HEVXjY9Y1il/nyT+G7NJvyDAA6jJAqd4gB6yl0HF9zhtL5fLEpwSObNki5p+J0oA5gRkAzWHTSH0ceiNuKk5e6arnPAXzCDbaYAN8J/Q== red6"
      ];
    };

    lerke = {
      uid = 1009;
      home = "/srv/users/lerke";
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAABJQAAAQEAvUZhccMXOGtRGDs/zOYIPKh4W74sMFWElv8/aNSkTlmlHVjRQHhLsLw8mDEYgAREFOhC1qlEeEuk50YfSUpLTvbmGBMqc7XGWw9Z24BXpFtoU+EZk4aWv7vBNm+aghspHCRdLqh6e/Q526XTeLovWNJU5JD/QWlFQIawq2BYGNwYrf1VEcisMn3ZLtMKsxJncbFwArcaWBzv5+f0Bza2x/FrHqRTBhVDzaeegr44g6mPSZuqOJ+HRKmZI8cfrEOQeZaQTri0kQ/Ip8a7OrbY5kyRUDmuh5K98CmCqbpaojPKLz6ELRMGpA/Rl84f12eaqw2t7R3ZG8QbZk+oobA8Ww=="
      ];
    };

    hackaru = {
      home = "/srv/hackaru";
      isNormalUser = true;
      extraGroups = [ "docker" ];
    };
  };
}
