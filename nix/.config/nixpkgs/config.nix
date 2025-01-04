let
  unstable = import (fetchTarball https://nixos.org/channels/nixos-unstable/nixexprs.tar.xz) { };
  stable = import (fetchTarball https://nixos.org/channels/nixos-24.11/nixexprs.tar.xz) { };
in
{

  allowUnfree = true;

  packageOverrides = _: with stable.pkgs; rec {

    myTexLive = texlive.combine {
      inherit (texlive) scheme-full;
    };

    all-env = [
      base-env
      tools-env
      nix-tools-env
      archivers-env
      emacs-env
      apps-env
      spelling-env
      development-env
      work-env
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
        alacritty
        stow
        trayer
        haskellPackages.xmobar
        xclip
        xbindkeys
        xdotool
        xorg.xinput
        xorg.xmodmap
        zile
        zsh
      ];
    };

    tools-env = buildEnv {
      name = "tools-env";
      paths = [
        appimage-run
        atuin
        bind
        binutils
        file
        ffmpeg
        gnupg
        gnome.gnome-keyring
        libxml2
        imagemagick
        parallel
        pamixer
        psmisc
        pinpoint
        inetutils
        tree
        xfce.tumbler
        which
        wget
        veracrypt
      ];
    };

    nix-tools-env = buildEnv {
      name = "nix-tools-env";
      paths = [
        cabal2nix
        dysnomia
        nix-generate-from-cpan
        nixpkgs-review
        nox
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
        emacs29
        emacs.pkgs.use-package
        emacs.pkgs.haskell-mode
        emacs.pkgs.scala-mode
#        emacs.pkgs.shm
        emacs.pkgs.writegood-mode
        emacs.pkgs.magit
        emacs.pkgs.nix-mode
        emacs.pkgs.markdown-mode
      ];
    };

    apps-env = buildEnv {
      name = "apps-env";
      paths = [
        calibre
        evince
        exercism
        feh
        filezilla
        gimp
        thunderbird
        k3b
        llpp
        libreoffice-fresh
        nextcloud-client
        obsidian
        phototonic
        pavucontrol
        simple-scan
        signal-desktop
        unstable.pkgs.super-productivity
        electrum
        viking
        vlc
        vscode
        ymuse
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
        pre-commit
        #idea.idea-ultimate
      ];
    };

    work-env = buildEnv {
      name = "work-env";
      paths = [
        #citrix_workspace
        docker
        maven
      ];
    };

  };
}
