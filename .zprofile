#!/usr/bin/env zsh

# Extending PATH
PATH=$PATH:$HOME/.cabal/bin:$HOME/.bin
export PATH

# Isabelle Options
PROOFGENERAL_OPTIONS="-m iff -m no_brackets"
export PROOFGENERAL_OPTIONS

# Specify isabelles pdf viewer
PDF_VIEWER=zathura
export PDF_VIEWER

# Starting X when login in on tty1
if [ "$(tty)" = "/dev/tty1" ]; then
    # Check for backup before login
    # and start ssh-agent with X
    backup && startx &!
    logout
fi
