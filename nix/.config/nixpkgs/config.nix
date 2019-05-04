{

  allowUnfree = true;

  packageOverrides = pkgs : with pkgs; rec {

   myTexLive = texlive.combine {
     inherit (texlive) scheme-full;
   };

   all-env = [
     base-env tools-env nix-tools-env archivers-env emacs-env apps-env
     spelling-env development-env security-env work-env
   ];

   base-env = buildEnv {
     name = "base-env";
     paths = [
       acpi
       bgs
       bmon
       dmenu
       dunst
       fsql
       i3lock
       inotify-tools
       htop
       libnotify
       networkmanagerapplet
       networkmanager-openconnect
       rxvt_unicode
       stow
       trayer
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
       #ghostscript
       libxml2
       nox
       imagemagick
       parallel
       psmisc
       pinpoint
       telnet
       tree
       xfce.tumbler
       xfce.ristretto
       which
       wget
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
       p7zip
     ];
   };

   emacs-env = buildEnv {
     name = "emacs-env";
     paths = [
       emacs
       emacsMelpa.haskell-mode
       emacsMelpa.scala-mode
       emacsMelpa.shm
       emacsMelpa.writegood-mode
       emacsMelpa.magit
       emacsMelpa.nix-mode
     ];
   };

   apps-env = buildEnv {
     name = "apps-env";
     paths = [
       areca

       calibre
       clementine
       evince
       exercism
       feh
       filezilla
       firefox
       homebank
       krusader
       thunderbird
       llpp
       libreoffice-fresh
       pass
       geeqie
       pcmanfm
       phototonic
       electrum
       quasselClient
       viking
       vlc
       zim
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
       gnupg1compat
       pinentry
     ];
   };

   work-env = buildEnv {
     name = "work-env";
     paths = [
       citrix_receiver
       docker
       hamster-time-tracker
        maven
     ];
   };
   
  };
}
