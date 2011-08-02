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
    *rxvt*)
        bindkey '\e[7~' beginning-of-line
        bindkey '\e[8~' end-of-line  
        bindkey '\eOc' forward-word  
        bindkey '\eOd' backward-word 
        bindkey '\e[3~' delete-char
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

# Move to where the arguments belong.
after-first-word() {
  zle beginning-of-line
  zle forward-word
}
zle -N after-first-word
bindkey "^@" after-first-word


# Keybindings to change pacman commands on the fly
# TODO: DRY!
replace-pacman-command() {
  BUFFER=${BUFFER/pacman [-a-zA-Z]#/pacman $@}
}

replace-pacman-command-insert() {
  replace-pacman-command "-S"
}

replace-pacman-command-search() {
  replace-pacman-command "-Ss"
}

replace-pacman-command-remove() {
  replace-pacman-command "-Rs"
}

replace-pacman-command-info() {
  replace-pacman-command "-Qi"
}

zle -N replace-pacman-command-insert
bindkey "^[i" replace-pacman-command-insert

zle -N replace-pacman-command-search
bindkey "^[f" replace-pacman-command-search

zle -N replace-pacman-command-remove
bindkey "^[r" replace-pacman-command-remove

zle -N replace-pacman-command-info
bindkey "^[p" replace-pacman-command-info
