# Aliases
alias todo="hsgtd"
alias ls="ls --color=auto"
alias xfiglatex="xfig -specialtext -latexfonts -startlatexFont default"

# Suffix aliases
alias -s pdf="background zathura"
alias -s djvu="background djview"

# Function to start suffix aliases in background
background(){
  $1 $2 &!
}

global-alias-space(){
   local ga="$LBUFFER[(w)-1]"
   [[ -n $ga ]] && LBUFFER[(w)-1]="${${galiases[$ga]}:-$ga}"
   zle self-insert
}

global-alias-tilde(){
   if [[ $LBUFFER = "git"* ]]; then
      LBUFFER+="~"
   else
      LBUFFER+="~/"
   fi
}

global-alias-dot() {
   if [[ $LBUFFER = *.. ]]; then
       LBUFFER+=/..
   else
       LBUFFER+=.
   fi
}

zle -N global-alias-space
zle -N global-alias-tilde
zle -N global-alias-dot

alias -g g="| grep"
alias -g p="| $PAGER"

bindkey ' ' global-alias-space
bindkey '~' global-alias-tilde
bindkey '.' global-alias-dot
