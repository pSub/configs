# Created by pSub for 4.3.10
setopt autocd
setopt no_beep
setopt prompt_subst
setopt function_argzero
setopt histignoredups

autoload -U compinit && compinit
autoload -U keeper && keeper
autoload -U colors && colors
autoload -Uz vcs_info
zmodload -ap zsh/mapfile mapfile 

# set some colors
for COLOR in RED GREEN YELLOW WHITE BLACK CYAN; do
    eval PR_$COLOR='%{$fg[${(L)COLOR}]%}'         
    eval PR_BRIGHT_$COLOR='%{$fg_bold[${(L)COLOR}]%}'
done                                                 
PR_RESET="%{${reset_color}%}"; 

FMT_BRANCH="${PR_GREEN}%s on %b%u%c in %S${PR_RESET}"
FMT_ACTION="(${PR_CYAN}%a${PR_RESET}%)"

zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr '¹'  # display ¹ if there are unstaged changes
zstyle ':vcs_info:*:prompt:*' stagedstr '²'    # display ² if there are staged changes
zstyle ':vcs_info:*:prompt:*' actionformats "${FMT_BRANCH}${FMT_ACTION}" ""
zstyle ':vcs_info:*:prompt:*' formats       "${FMT_BRANCH}"              ""

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
   su --command="/etc/init.d/$1 $0"
}

sup sup-add sup-cmd sup-config sup-dump sup-recover-sources sup-sync(){
  ruby -I $HOME/.gem/mainline/lib -w $HOME/.gem/mainline/bin/$0
}

sync-unison(){
    unison unison && unison all;
}

hexeditor(){
   xxd $1 | vipe | xxd -r | sponge $1
}


precmd() {       
    vcs_info 'prompt'          
}

git='$vcs_info_msg_0_' 

PROMPT="[%n@%m %c]%1(j.(%j%).)%1(?.%{$fg[red]%}.)%#%{$reset_color%} "
RPROMPT="${git}"

export PAGER=less

eval `dircolors`
alias when="when --futur=0 --past=0"
alias todo="hsgtd"
alias ls="ls --color=auto"
alias -g g="| grep"
alias -g p="| $PAGER"
alias sup="ruby -I $HOME/.gem/mainline/lib -w $HOME/.gem/mainline/bin/sup"

if [ "$(tty)" = "/dev/tty1" ]; then
   ssh-agent startx &!
   logout
fi

zle -N global-alias-space
zle -N global-alias-tilde
zle -N global-alias-dot
zstyle ':completion::complete:*' rehash true
zstyle ':completion:*:kill:*' command 'ps cf -u $USER -o pid,%cpu,cmd'

bindkey ' ' global-alias-space
bindkey '~' global-alias-tilde
bindkey '.' global-alias-dot
