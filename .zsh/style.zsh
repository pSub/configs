# -*- mode: shell-script -*-

# vcs_info preferences
zstyle ':vcs_info:*' enable git hg darcs cvs svn
zstyle ':vcs_info:*' max-exports "4"
zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr "%{$fg[yellow]%}★%{$reset_color%}"
zstyle ':vcs_info:*:prompt:*' stagedstr "%{$fg[green]%}★%{$reset_color%}"
zstyle ':vcs_info:*:prompt:*' actionformats '%a'
zstyle ':vcs_info:*:prompt:*' formats '%u%c' '%s' "on %{$fg[red]%}%b%{$reset_color%}" 'in %S'

# Completion style
zstyle ':completion::complete:*' rehash true
zstyle ':completion:*' menu select
zstyle ':completion:*:kill:*' command 'ps cf -u $USER -o pid,%cpu,cmd'
zstyle -e ':completion:*:-command-:*:commands' list-colors 'reply=( '\''=(#b)('\''$words[CURRENT]'\''|)*-- #(*)=0=38;5;45=38;5;136'\'' '\''=(#b)('\''$words[CURRENT]'\''|)*=0=38;5;45'\'' )'
zstyle ':completion:retract::::' completer _retract

# Standard unison completion for the unison-sync wrapper
compdef _unison unison-sync

# pasting with tabs does not perform completion
zstyle ':completion:*' insert-tab pending
