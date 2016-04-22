{

  allowUnfree = true;

  packageOverrides = pkgs : with pkgs; {
  
   myTexLive = texLiveAggregationFun {
     paths = [
       texLive texLiveExtra texLiveBeamer texLiveCMSuper #mathpartir
     ];
   };

   base-env = buildEnv {
     name = "base-env";
     paths = [
       acpi
       bgs
       bmon
       dmenu
       dunst
       i3lock
       inotify-tools
       htop
       networkmanagerapplet
       rxvt_unicode
       stow
       trayer
       unison
       haskellPackages.xmobar
       xclip
       xbindkeys
       xdotool
       xlibs.xinput
       xlibs.xmodmap
       zile
     ];
   };

   tools-env = buildEnv {
     name = "tools-env";
     paths = [
       bind
       binutils
       file
       ghostscript
       nox
       nix-repl
       mc
       imagemagick
       parallel
       psmisc
       pinpoint
       traceroute
       telnet
       tree
       xfce.tumbler
       xfce.ristretto
       which
       zsh-navigation-tools
     ];
   };

   nix-tools-env = buildEnv {
     name = "nix-tools-env";
     paths = [
       cabal2nix
       dysnomia
       nix-generate-from-cpan
       nixops
       nixpkgs-lint
     ];
   };

   archivers-env = buildEnv {
     name = "archivers-env";
     paths = [
       atool
       zip
       unzip
     ];
   };

   emacs-env = buildEnv {
     name = "emacs-env";
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
     name = "apps-env";
     paths = [
       areca
       calibre
       chromium
       evince
       feh
       thunderbird
       llpp
       pass
       geeqie
       gnucash
       pcmanfm
       phototonic
       electrum
       quasselClient_qt5
     ];
   };

   spelling-env = buildEnv {
     name = "spelling-env";
     paths = [
       aspell
       aspellDicts.de
       aspellDicts.en
     ];
   };

   development-env = buildEnv {
     name = "development-env";
     paths = [
       gitFull
       idea.idea-ultimate
       subversion
     ];
   };

   security-env = buildEnv {
     name = "security-env";
     paths = [
       apg
       gnupg
       pinentry
     ];
   };
   
  };
}





