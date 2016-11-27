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
export ZSHFUN=$ZSHDIR/functions
export EDITOR="ec"
export PAGER="less -R"
export BROWSER=dwb
export PRESENTER=pdfpc
export REPORTTIME="10"
export HISTSIZE=1000
export SAVEHIST=1000
export HISTFILE=$HOME/.zshhistory
export COLORTERM=yes
export DIRSTACKSIZE=9
export DIRSTACKFILE=~/.zdirs

# MODULES
autoload -U compinit && compinit
autoload -U keeper && keeper
autoload -U colors && colors
autoload -U zmv
autoload -Uz vcs_info
autoload -U url-quote-magic

# Add directory with custom functions to FPATH
fpath=($fpath $ZSHFUN)

# Mark all functions in ZSHFUN for autoloading
for file in $ZSHFUN/*
do
    autoload -U $file:t
done

# Register zle widgets
zle -N self-insert url-quote-magic
zle -N global-alias-space
zle -N global-alias-tilde
zle -N global-alias-dot
zle -N global-alias-dirstack
zle -N after-first-word
zle -N goto-directory
zle -N find-file
zle -C retract complete-word _generic

# IRC client like history
# http://zshwiki.org/home/zle/ircclientlikeinput
zle -N _fake-accept-line
zle -N down-line-or-history _down-or-fake-accept-line

# OPTIONS

# change directory without 'cd'
setopt autocd

# Push direcotries automatically
setopt autopushd

# Ignore duplicates on dictionary stack
setopt pushd_ignoredups

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

# Do hashing
setopt hash_cmds
setopt hash_dirs

# Redirect output to multiple destinations (this is the default)
setopt multios


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

# Persistent directory stack by Christian Neukirchen
# <http://chneukirchen.org/blog/archive/2012/02/10-new-zsh-tricks-you-may-not-know.html>
if [[ -f $DIRSTACKFILE ]] && [[ $#dirstack -eq 0 ]]; then
  dirstack=( ${(f)"$(< $DIRSTACKFILE)"} )
  [[ -d $dirstack[1] ]] && cd $dirstack[1] && cd $OLDPWD
fi
chpwd() {
  print -l $PWD ${(u)dirstack} >$DIRSTACKFILE
}

# Load config files
if [[ -d $ZSHDIR ]] {
    for config_file in $config_files
    do
        if [[ -f $ZSHDIR/$config_file.zsh ]] {
            source $ZSHDIR/$config_file.zsh
        }
    done
}

unset config_files


### ZNT's installer added snippet ###
fpath=( "$fpath[@]" "$HOME/.config/znt/zsh-navigation-tools" )
autoload n-aliases n-cd n-env n-functions n-history n-kill n-list n-list-draw n-list-input n-options n-panelize n-help
autoload znt-usetty-wrapper znt-history-widget znt-cd-widget znt-kill-widget
alias naliases=n-aliases ncd=n-cd nenv=n-env nfunctions=n-functions nhistory=n-history
alias nkill=n-kill noptions=n-options npanelize=n-panelize nhelp=n-help
zle -N znt-history-widget
bindkey '^R' znt-history-widget
setopt AUTO_PUSHD HIST_IGNORE_DUPS PUSHD_IGNORE_DUPS
zstyle ':completion::complete:n-kill::bits' matcher 'r:|=** l:|=*'
### END ###
