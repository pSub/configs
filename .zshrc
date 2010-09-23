# Created by pSub for 4.3.10
setopt autocd
setopt no_beep
setopt prompt_subst
setopt function_argzero
setopt histignoredups

autoload -U compinit && compinit
autoload -U keeper && keeper
autoload -U colors && colors
autoload -U zfinit && zfinit
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
   if [[ ! -f $1 ]]; then
        touch $1
   fi
   input=$1
   output=$2
   if [ $# -eq 1 ]; then
      output=$1
   fi
   xxd $input | vipe | xxd -r | sponge $output
}

greptodos(){
   find . -name $1 | xargs grep --no-filename -oE 'TODO:[^$]*' | sed s/TODO:/$2/ | combine - xor ~/.todo.txt | sponge ~/.todo.txt
}

addExpense(){
  echo $1 >> $HOME/finances/$(date +"%Y-%m")/out
}

addRevenue(){
  echo $1 >> $HOME/finances/$(date +"%Y-%m")/in
}

showFinancialPosition(){
   totalIn=$(cat $HOME/finances/$(date +"%Y-%m")/in | awk -F ";" '{SUM += $2} END {print SUM}')
   totalOut=$(cat $HOME/finances/$(date +"%Y-%m")/out | awk -F ";" '{SUM += $2} END {print SUM}')
   total=$(($totalIn - $totalOut))
   printf "Revenues: %.2f\nExpenses: %.2f\nTotal:    %.2f\n" "$totalIn" "$totalOut" "$total"
}

precmd() {       
    vcs_info 'prompt'          
}

git='$vcs_info_msg_0_' 

PROMPT="[%n@%m %c]%1(j.(%j%).)%1(?.%{$fg[red]%}.)%#%{$reset_color%} "
RPROMPT="${git}"

export PAGER=less
export REPORTTIME="10"

eval `dircolors`
alias when="when --futur=0 --past=0"
alias todo="hsgtd"
alias ls="ls --color=auto"
alias -g g="| grep"
alias -g p="| $PAGER"
alias -s pdf=apvlv
alias -s djvu=djview
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
