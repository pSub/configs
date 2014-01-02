
{

  dwm.patches = [ ./dwm-sync.patch ];

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
       pkgs.texLive pkgs.texLiveExtra pkgs.texLiveBeamer pkgs.texLiveCMSuper
     ];  name = "mytexlive"; };

   # Collection packages required to use my configuration files
   myPackages = pkgs.buildEnv {
     name = "myPackages";
     paths = [
       acpi
       aspell
       aspellDicts.de
       aspellDicts.en
       atool
       bgs
       bmon
       calibre
       dmenu
       dunst
       dwb
       #dwm
       git
       gnupg
       htop
       i3lock
       inotifyTools
       pinentry
       rxvt_unicode
       thunderbird
       udisks_glue
       unison
       weechat
       wmname

       # Emacs
       emacs24
       emacs24Packages.autoComplete
       emacs24Packages.haskellMode
       emacs24Packages.magit
       emacs24Packages.org
       emacs24Packages.scalaMode

       # Xutils
       xbindkeys
       xlibs.xinput
       xlibs.xmodmap

        # Misc
       haskellPackages.Agda
     ];
   };

   myDevel = pkgs.buildEnv {
     name = "myDevel";
     paths = [
       eclipses.eclipse_sdk_431
       subversion
       mercurial
     ];
   };

   myNixDevel = pkgs.buildEnv {
     name = "myNixDevel";
     paths = [
       nixpkgs-lint
     ];
   };
   
  };
}
