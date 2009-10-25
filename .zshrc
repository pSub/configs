# Created by pSub for 4.3.10
global-alias-space(){
   local ga="$LBUFFER[(w)-1]"
   [[ -n $ga ]] && LBUFFER[(w)-1]="${${galiases[$ga]}:-$ga}"
   zle self-insert
}

PROMPT="[%n@%m %c]%1(j.(%j%).)%# "
export PAGER=less

eval `dircolors`
alias when="when --futur=0 --past=0"
alias ls="ls --color=auto"
alias -g G="| grep"
alias -g P="| $PAGER"

if [ "$(tty)" = "/dev/tty1" ]; then
   (startx -- -nolisten tcp  &) && exit
fi

setopt autocd
setopt no_beep
setopt rm_star_wait
autoload -U compinit && compinit
zle -N global-alias-space
zstyle ':completion::complete:*' rehash true
zstyle ':completion:*:kill:*' command 'ps cf -u $USER -o pid,%cpu,cmd'

bindkey ' ' global-alias-space
