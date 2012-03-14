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
alias -s pdf="background zathura"
alias -s djvu="background zathura"
alias -s ps="background zathura"

# Global aliases
alias -g g="| grep"
alias -g p="| $PAGER"

# Function to start suffix aliases in background
background(){
  $1 $2 &!
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

# Register these functions as zle-widgets
zle -N global-alias-space
zle -N global-alias-tilde
zle -N global-alias-dot

# Bind the appropriated keys to the expansion
# widgets above.
bindkey ' ' global-alias-space
bindkey '~' global-alias-tilde
bindkey '.' global-alias-dot
