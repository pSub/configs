#! /usr/bin/env nix-shell
#! nix-shell -i zsh -p nixos-rebuild

# This script deploys from an arbitrary computer to an ARM64 server.

# FIXME: I call nixos-rebuild twice, with slightly different quoting, because there is some problem with quotes in nixos-rebuild.

nixos-rebuild switch --verbose --fast --flake ".#nixos" --use-remote-sudo --target-host "deployer@152.53.0.129" --build-host "deployer@152.53.0.129" --builders "ssh://deployer@152.53.0.129 aarch64-linux"

nixos-rebuild switch --verbose --fast --flake ".#nixos" --use-remote-sudo --target-host "deployer@152.53.0.129" --build-host "deployer@152.53.0.129" --builders '"ssh://deployer@152.53.0.129 aarch64-linux"'
