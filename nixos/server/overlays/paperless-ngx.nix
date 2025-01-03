final:  prev :
{
  paperless-ngx-without-tests = prev.paperless-ngx.overrideAttrs (oldAttrs: {
    # Disable tests until https://github.com/NixOS/nixpkgs/issues/361006 is resolved.
    doInstallCheck = false;
  });
}