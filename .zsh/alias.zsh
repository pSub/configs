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

# Register functions for aliases as zle-widgets
zle -N global-alias-space
zle -N global-alias-tilde
zle -N global-alias-dot
zle -N global-alias-dirstack

# Bind the appropriated keys to the expansion
# widgets above.
bindkey ' ' global-alias-space
bindkey '~' global-alias-tilde
bindkey '.' global-alias-dot
bindkey '^X^f' global-alias-dirstack
