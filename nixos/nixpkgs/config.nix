{

  allowUnfree = true;

  packageOverrides = pkgs : with pkgs; {
  
   myTexLive = texLiveAggregationFun {
     paths = [
       texLive texLiveExtra texLiveBeamer texLiveCMSuper #mathpartir
     ];
   };

   base-env = buildEnv {
     name = "base";
     paths = [
       acpi
       atool
       bgs
       bmon
       dmenu
       dunst
       i3lock
       inotify-tools
       htop
       rxvt_unicode
       unison
       haskellPackages.xmobar
       xclip
       xbindkeys
       xlibs.xinput
       xlibs.xmodmap
       zile
     ];
   };

   tools-env = buildEnv {
     name = "tools-env";
     paths = [
       psmisc
     ];
   };

   emacs-env = buildEnv {
     name = "emacs";
     paths = [
       emacs24
       emacs24Packages.autoComplete
       emacs24Packages.haskellMode
       emacs24Packages.magit
       emacs24Packages.org
       emacs24Packages.scalaMode2
       emacs24Packages.structuredHaskellMode
       emacs24Packages.writeGood
     ];
   };

   apps-env = buildEnv {
     name = "apps";
     paths = [
       calibre
       feh
       firefox
       thunderbird
       pass
       quasselClient_qt5
     ];
   };

   spelling-env = buildEnv {
     name = "spelling";
     paths = [
       aspell
       aspellDicts.de
       aspellDicts.en
     ];
   };

   development-env = buildEnv {
     name = "development";
     paths = [
       gitFull
     ];
   };

   security-env = buildEnv {
     name = "security";
     paths = [
       apg
       gnupg
       pinentry
     ];
   };
   
  };
}





