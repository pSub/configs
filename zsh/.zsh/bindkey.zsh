# -*- mode: shell-script -*-

# Term-specific bindings
case "$TERM" in
    linux)
        bindkey '\e[1~' beginning-of-line
        bindkey '\e[4~' end-of-line
        bindkey '\e[3~' delete-char
        bindkey '\e[2~' overwrite-mode
        bindkey '\e[C' forward-word
        bindkey '\e[D' backward-word
        ;;
    *screen*)
        # In Linux console
        bindkey '\e[1~' beginning-of-line
        bindkey '\e[4~' end-of-line
        bindkey '\e[3~' delete-char
        bindkey '\e[2~' overwrite-mode
        # Just in rxvt
        bindkey '\eOc' forward-word
        bindkey '\eOd' backward-word
        #For use in xterm
        #bindkey '\e[1;5C' forward-word
        #bindkey '\e[1;5D' backward-word
        ;;
    *alacritty*)
        bindkey '\e[H'  beginning-of-line
        bindkey '\e[F'  end-of-line
        bindkey '\e[3~' delete-char

        bindkey '\eOc' forward-word
        bindkey '\eOd' backward-word
        bindkey '\e[2~' overwrite-mode
        ;;
    xterm*)
        bindkey '\e[1~' beginning-of-line
        bindkey '\e[4~' end-of-line
        bindkey '\e[3~' delete-char
        bindkey '\e[2~' overwrite-mode
        bindkey '\e[1;5C' forward-word
        bindkey '\e[1;5D' backward-word
        ;;
esac

bindkey "^@" after-first-word
bindkey ' ' global-alias-space
bindkey '~' global-alias-tilde
bindkey '.' global-alias-dot
bindkey '^X^g' goto-directory
bindkey '^X^f' find-file
bindkey '^H' retract

## bind to shift-tab
bindkey -s '^[[Z' 'urxvtc -T floatwin &!\n'
