{
  sops.secrets = {
    "mtls/actual" = {
      sopsFile = ./mtls/actual.yaml;
      format = "binary";
      owner = "nginx";
     };
    "mtls/adguard" = {
      sopsFile = ./mtls/adguard.yaml;
      format = "binary";
      owner = "nginx";
     };
    "mtls/netdata" = {
      sopsFile = ./mtls/netdata.yaml;
      format = "binary";
      owner = "nginx";
     };
    "mtls/paperless" = {
      sopsFile = ./mtls/paperless.yaml;
      format = "binary";
      owner = "nginx";
     };
    "mtls/vaultwarden" = {
      sopsFile = ./mtls/vaultwarden.yaml;
      format = "binary";
      owner = "nginx";
     };
  };
}
