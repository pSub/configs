# Pascal's Configuration Files

These are the configuration files I actively use. I use them to set up my
notebook as well as my small server.

## Characteristic Features

* [Xmonad] When a window is in focus the mouse cursor is placed in the center of the window.
* [NixOps] `system.autoUpgrade` plays nicely together with NixOps (my take on NixOS/nixops#842).
* [NixOps] Module `systemd-email-notify` that notifies via email when any service fails.
* [Nixpkgs] Packages are installed in custom environments that are combined in an attribute `all-env`.
* [Nixpkgs] The channel (nixpkgs version) is defined in `config.nix`.
* [Xmodmap] Capslock is mapped to (left) control.

## Installation

As this repository contains configurations for multiple systems, there is no
way to install them all at once.

### Dot Files

You can install the dot files via a [GNU stow](https://www.gnu.org/software/stow/)
wrapper. Note that the wrapper requires [Nix](https://nixos.org/nix/). Calling the
wrapper without any additional arguments _stows_ (ie. soft-links) the dot files
into your home directory.

``` shell
./stow-it
```

All arguments you pass to the wrapper are passed directly to GNU stow. This allows
you to preview what the installation will look like

``` shell
./stow-it --stow --simulate
```

to update an existing installation

``` shell
./stow-it --restow
```

or to remove all links that were created by GNU stow.

``` shell
./stow-it --delete
```

### NixOS Configuration

I have one [NixOS](https://nixos.org/nixos/) [configuration](nixos/configuration.nix)
file and separate files with hardware configurations for different notebooks. As
this is structure is very static you have to copy the files by hand to use my
configuration.

### NixOps Configuration

Currently I am managing one server via [NixOps](https://nixos.org/nixops/). With a
little bit of tweaking you should get this configuration running. Some things that
you will definitely need to adapt: Keys / passwords, hostname and device names.
