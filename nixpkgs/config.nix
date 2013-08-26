
{

  dwm.patches = [ ./dwm.patch ];

  packageOverrides = pkgs : with pkgs; {
    sdlEnv = pkgs.myEnvFun {
      name = "sdl";
      buildInputs = [ stdenv ];
    };

    dwbEnv = pkgs.myEnvFun {
      name = "dwb";
      buildInputs = [ stdenv libsoup webkit pkgconfig gnome3.gtk gnutls json_c m4 ];
    };

    # How can I give that environment a name?
    # Should this be turned into a real environment?
    haskell_env = pkgs.haskellPackages.ghcWithPackages (pkgs : [
         pkgs.haskellPlatform
         stdenv
         zlib
         haskellPackages.cabalDev
         haskellPackages.hlint
     ]);

  };
}