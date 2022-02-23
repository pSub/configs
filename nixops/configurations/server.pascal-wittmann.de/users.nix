{ config, pkgs, ... }:

{
  users.groups = {
    mail = {};
  };

  users.extraUsers = {
    root.openssh.authorizedKeys.keys = [
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj pascal@brauchli"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFMsKQmC81HP+WrkINeslvRe7BWkx0a2e5F7BRDLTXGI NixOps client key for server"
      "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCz/EsycVbiWMIhIOjJICo5uiCI7glX50C3y8cszY372o6VXY/hsT6RLM07Ch2CbhumUJnjJrxFG/GhiI3Hes16BrAHyGlWu3jLowJXSqWa3Z5X2vWNrQXrR5RH66vFiaWCgbZX9mH9rcFW49UYSssuHgBNzO2O/a4nwT8z7qeB4MGZr684PKAmxTwRFp9NTDnQsKMlGY0kTGfbBZ5Ms/HMeDDsCqkitfYl4Sv29ku6N3OMUgCbsWLn4p0PC7KJq540B15n3ct2UVi2J/rzSNt213NzL5pfRgzK18nHqXNVdVY2yuWAfxEfOiy5B+nKunrRZ9CcS4zK8y3vWp8BZJk4xzKIqp7vrS2h7AS58kOMwQA6gxTgACmsmqTacCzjci1aKu96cKxy2Z1ucJOsG/SMva2OydnR0VDnrgsAUKFdTAfzya508RJ0nhWWnhrQqUqzWYpwuQQY3Yi9bOnDMn2MNWcmAnxVOV7fY/btqgW1Z/NIfvD9gqg29Q0HMgAzxaHVNb0vJf9Xlq9JQFdBjTLLUwF4hrIxa0fyUdsNLSPajt7XKN61CeYFk5JhbXvh1NWIN8HEVXjY9Y1il/nyT+G7NJvyDAA6jJAqd4gB6yl0HF9zhtL5fLEpwSObNki5p+J0oA5gRkAzWHTSH0ceiNuKk5e6arnPAXzCDbaYAN8J/Q== red6"
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
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQD2nAZ2QKEF4cArMUSgOXg3y9Xz0eh6SEuvCC1p+ImkfqlSa4H4We0mLTPvfniSP4NAH5heDMZyxW9DEHQFfXmkHk6eICaJfqdHeyuhL54+l4PvmsWRP9YUKt5ocQBFlUsCQ3q+G4eQcEo342HLDe6+ITkd9uUGSyOuCkabRrU4KPl44B6R4UOJi86qw1PnINd3EA7WzbdFBSCj/6ZsTYW8LNMcKgOUOiXf5cCnOGUV6Ib79Rn85u36/71kbd4zN+e+7WjUVdsnNgCtEs3bCsRI1mwuKAeqkRrDkiFUDmzBMTcNBKITuTNBWEpWXuZmAFGRazNVLiVq4mejR0duLKgj pascal@brauchli"
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
  };
}
