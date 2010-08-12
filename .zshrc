# Created by pSub for 4.3.10
setopt autocd
setopt no_beep
setopt function_argzero
setopt histignoredups

autoload -U compinit && compinit
autoload -U keeper && keeper
autoload -U colors && colors
zmodload -ap zsh/mapfile mapfile 

global-alias-space(){
   local ga="$LBUFFER[(w)-1]"
   [[ -n $ga ]] && LBUFFER[(w)-1]="${${galiases[$ga]}:-$ga}"
   zle self-insert
}

global-alias-tilde(){
   LBUFFER+="~/"
}

global-alias-dot() {
   if [[ $LBUFFER = *.. ]]; then
       LBUFFER+=/..
   else
       LBUFFER+=.
   fi
}

start restart stop reload(){
   su --command="/etc/rc.d/$1 $0"
}

sync-unison(){
   unison unison && unison all;
}

PROMPT="[%n@%m %c]%1(j.(%j%).)%1(?.%{$fg[red]%}.)%#%{$reset_color%} "

export PAGER=less

eval `dircolors`
alias when="when --futur=0 --past=0"
alias todo="hsgtd"
alias ls="ls --color=auto"
alias -g g="| grep"
alias -g p="| $PAGER"

if [ "$(tty)" = "/dev/tty1" ]; then
   (startx -- -nolisten tcp  &) && exit
fi

zle -N global-alias-space
zle -N global-alias-tilde
zle -N global-alias-dot
zstyle ':completion::complete:*' rehash true
zstyle ':completion:*:kill:*' command 'ps cf -u $USER -o pid,%cpu,cmd'

bindkey ' ' global-alias-space
bindkey '~' global-alias-tilde
bindkey '.' global-alias-dot
