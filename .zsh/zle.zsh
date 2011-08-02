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
