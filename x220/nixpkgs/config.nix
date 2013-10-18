
{

  dwm.patches = [ ./dwm.patch ];

  packageOverrides = pkgs : with pkgs; {
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

   myTexLive = pkgs.texLiveAggregationFun {
     paths = [
       texLive texLiveExtra texLiveBeamer
     ];  name = "mytexlive"; };
  };
}
