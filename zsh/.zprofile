#!/usr/bin/env zsh

# Extending PATH
PATH=$PATH:$HOME/.nix-profile/bin:$HOME/.cabal/bin:$HOME/bin:
export PATH

# Isabelle Options
PROOFGENERAL_OPTIONS="-m iff -m no_brackets"
export PROOFGENERAL_OPTIONS

# Specify isabelles pdf viewer
PDF_VIEWER=llpp
export PDF_VIEWER
