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

# Function to start suffix aliases in background
background(){
  $1 $2 &> /dev/null &!
}

# Function to start suffix aliases in background with compressed files
background-compressed(){
  arguments=""
  for arg in $2; do
    arguments+="=(zcat $arg) "
  done
  $1 $arguments &> /dev/null &!
}

# Expand an global alias when hitting space after the alias
global-alias-space(){
   local ga="$LBUFFER[(w)-1]"
   [[ -n $ga ]] && LBUFFER[(w)-1]="${${galiases[$ga]}:-$ga}"
   zle self-insert
}

# Add a slash after tilde, but only if git isn't involved.
# Git is the only case I need a tilde without a slash.
global-alias-tilde(){
   if [[ $LBUFFER = "git"* ]]; then
      LBUFFER+="~"
   else
      LBUFFER+="~/"
   fi
}

# Expand dots. E.g. ... -> ../..
global-alias-dot() {
   if [[ $LBUFFER = *.. ]]; then
       LBUFFER+=/..
   else
       LBUFFER+=.
   fi
}

# Handy use of dirstack bound to C-x C-f
global-alias-dirstack() {
   LBUFFER+="cd -"
   zle expand-or-complete
}

# Register these functions as zle-widgets
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
