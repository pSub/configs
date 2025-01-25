{
  sops.secrets = {
    "mtls/paperless" = {
      sopsFile = ./mtls/paperless.yaml;
      format = "yaml";
      owner = "nginx";
     };
  };
}
