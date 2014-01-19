#!/usr/bin/env zsh

# Extending PATH
PATH=$PATH:$HOME/.cabal/bin:$HOME/bin
export PATH

# Isabelle Options
PROOFGENERAL_OPTIONS="-m iff -m no_brackets"
export PROOFGENERAL_OPTIONS

# Specify isabelles pdf viewer
PDF_VIEWER=zathura
export PDF_VIEWER

GTK_IM_MODULE=xim
export GTK_IM_MODULE
