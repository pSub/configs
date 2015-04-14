
{

  allowUnfree = true;
  dwb.enableAdobeFlash = true;

  packageOverrides = pkgs : with pkgs; {

   myTexLive = texLiveAggregationFun {
     paths = [
       texLive texLiveExtra texLiveBeamer texLiveCMSuper
     ];
   };

   quasselClient = kde4.quassel.override { withKDE = false;
        monolithic = false;
        client = true;
        tag = "-client";
   };

   ktouchWrapped = kde4.wrapper kde4.ktouch;

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
       gitFull
       gnupg
       htop
       i3lock
       inotifyTools
       pinentry
       rxvt_unicode
       thunderbird-bin
       transmission_remote_gtk
       udisks_glue
       mpc_cli
       unison
       unzip
       weechat
       vim

       # Emacs
       emacs24
       emacs24Packages.autoComplete
       emacs24Packages.haskellMode
       emacs24Packages.magit
       emacs24Packages.org
       emacs24Packages.scalaMode2
       emacs24Packages.structuredHaskellMode
       emacs24Packages.writeGood

       # Xutils
       xclip
       xbindkeys
       xlibs.xinput
       xlibs.xmodmap

       # Misc
       haskellPackages.zlib
       haskellPackages.Agda
     ];
   };
   
  };
}
