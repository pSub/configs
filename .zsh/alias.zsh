# -*- mode: shell-script -*-

# Aliases
alias todo="hsgtd"
alias ls="ls --color=auto"
alias xfiglatex="xfig -specialtext -latexfonts -startlatexFont default"
alias unpack="aunpack"
alias pack="apack"
alias sync-unison="unison unison && unison all"
alias su="su --login"
alias ssh="TERM=xterm ssh"

# Suffix aliases
alias -s {pdf,djvu,ps}="background zathura"

# Global aliases
alias -g g="| grep"
alias -g p="| $PAGER"
