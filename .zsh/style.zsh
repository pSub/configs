# -*- mode: shell-script -*-

# vcs_info preferences
zstyle ':vcs_info:*' enable git hg darcs cvs svn
zstyle ':vcs_info:*' max-exports "4"
zstyle ':vcs_info:*:prompt:*' check-for-changes true
zstyle ':vcs_info:*:prompt:*' unstagedstr "%F{11}★%{$reset_color%}"
zstyle ':vcs_info:*:prompt:*' stagedstr "%F{28}★%{$reset_color%}"
zstyle ':vcs_info:*:prompt:*' actionformats '%a'
zstyle ':vcs_info:*:prompt:*' formats '%u%c' '%s' "on %{$fg[red]%}%b%{$reset_color%}" 'in %S'

# Completion style
zstyle ':completion::complete:*' rehash true
zstyle ':completion:*:kill:*' command 'ps cf -u $USER -o pid,%cpu,cmd'
