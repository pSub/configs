
{

  dwm.patches = [ ./dwm.patch ];

  packageOverrides = pkgs : with pkgs; {
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
