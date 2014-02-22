
{

  packageOverrides = pkgs : with pkgs; {

   myTexLive = pkgs.texLiveAggregationFun {
     paths = [
       texLive texLiveExtra texLiveBeamer texLiveCMSuper
     ];
     name = "mytexlive";
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
       thunderbird
       transmission_remote_gtk
       udisks_glue
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
       emacs24Packages.scalaMode
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
