{

  allowUnfree = true;

  packageOverrides = pkgs : with pkgs; {

   myTexLive = texlive.combine {
     inherit (texlive) scheme-basic koma-script babel-german todonotes xkeyval
       xcolor collection-pictures ms;
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
       emacs
       emacsMelpa.haskell-mode
       emacsMelpa.scala-mode
       emacsMelpa.shm
       emacsMelpa.writegood-mode
       emacsMelpa.magit
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
       quasselClient
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





