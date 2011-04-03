# Created by pSub for 4.3.10

export ZSHDIR=$HOME/.zsh
export EDITOR=vim
export PAGER=less
export REPORTTIME="10"
export HISTFILE=$HOME/.zshhistory
export COLORTERM=yes

# Load extra modules
autoload -U compinit && compinit
autoload -U keeper && keeper
autoload -U colors && colors
autoload -U zfinit && zfinit
autoload -U zmv
autoload -Uz vcs_info
zmodload -ap zsh/mapfile mapfile 

# Set Options
setopt autocd
setopt no_beep
setopt prompt_subst
setopt function_argzero
setopt histignoredups
setopt extendedglob
setopt interactivecomments

# Colors
if [[ -f ~/.dircolors ]] {
    if [[ ${TERM} == screen* ]] {
        eval $( TERM=screen dircolors ~/.dircolors )
    } else {
        eval $( dircolors ~/.dircolors )
    }
} else {
    eval $( dircolors -b )
}

function load_config() {
    if [[ -f $1 ]] {
        source $1
    }
}

if [[ -d $ZSHDIR ]] {
    load_config $ZSHDIR/alias.zsh
    load_config $ZSHDIR/bindkey.zsh
    load_config $ZSHDIR/functions.zsh
    load_config $ZSHDIR/style.zsh
    load_config $ZSHDIR/prompt.zsh
}

if [ "$(tty)" = "/dev/tty1" ]; then
   backup && ssh-agent startx &!
   logout
fi
