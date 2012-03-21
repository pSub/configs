# Created by pSub for 4.3.10

# Declaration of config files to
# be used, which are located in $ZSHDIR
config_files=(alias
              bindkey
              functions
              style
              prompt
              zle
             )

export ZSHDIR=$HOME/.zsh
export EDITOR="ec"
export PAGER=less
export REPORTTIME="10"
export HISTFILE=$HOME/.zshhistory
export COLORTERM=yes

# MODULES
autoload -U compinit && compinit
autoload -U keeper && keeper
autoload -U colors && colors
autoload -U zfinit && zfinit
autoload -U zmv
autoload -Uz vcs_info
zmodload -ap zsh/mapfile mapfile

# OPTIONS

# change directory without 'cd'
setopt autocd

# Push direcotries automatically
setopt autopushd

# Be quiet
setopt nobeep
setopt nohistbeep
setopt nolistbeep

# Maybe needed for prompt, I'm not sure
setopt prompt_subst

# $0 is the name of the function/script
setopt function_argzero

# No duplicate entries in history
setopt histignoredups

# Cool globbing stuff, see http://zsh.sourceforge.net/Intro/intro_2.html
setopt extendedglob

# Comments are allowed in the prompt, useful when pasting a shelscript
setopt interactivecomments


# COLORS
if [[ -f ~/.dircolors ]] {
    if [[ ${TERM} == screen* ]] {
        eval $( TERM=screen dircolors ~/.dircolors )
    } else {
        eval $( dircolors ~/.dircolors )
    }
} else {
    eval $( dircolors -b )
}

# Auxiliary function
function load_config() {
    if [[ -f $1 ]] {
        source $1
    }
}

# Load config files
if [[ -d $ZSHDIR ]] {
    for config_file in $config_files
    do
      load_config $ZSHDIR/$config_file.zsh
    done
}

unfunction load_config
unset config_files
