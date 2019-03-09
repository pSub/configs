# -*- mode: shell-script -*-

# Aliases
alias ls="ls --color=auto"
alias xfiglatex="xfig -specialtext -latexfonts -startlatexFont default"
alias unpack="aunpack"
alias pack="apack"
alias sul="su --login"
alias filemenu='ls | slmenu -p $PWD; [ $? -ne 0 ] && echo $PWD'
alias pdfviewer='llpp.inotify'

# Suffix aliases
alias -s {djvu,ps}="background zathura"
alias -s pdf="background pdfviewer"

# Global aliases
alias -g g="| grep"
alias -g p="| $PAGER"
