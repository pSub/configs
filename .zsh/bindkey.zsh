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
pacman_bindings=("^[i" "S"     # Meta-i → install
                 "^[f" "Ss"    # Meta-f → search
                 "^[r" "Rs"    # Meta-r → remove
                 "^[p" "Qi"    # Meta-p → info / properties
                )

# Function that checks the BUFFER for a pacman command
# and replaces the operation with the given parameters
replace-pacman-command() {
      if [[ $LBUFFER = "pacman"* ]]; then
         CURSOR=0
         zle forward-word
         zle delete-word
         LBUFFER+="$*"
         zle end-of-line
      fi
}

# Creates widgets from pacman_bindings and replace-pacman-command
# and binds them the the given keys
local i
i=1
while [ $i -le ${#pacman_bindings} ]; do

    # The widget function: Operation is accessed over the name of
    # this function because its hard to create a local accessor
    # inside the function.
    function "replace-pacman-command-"$i {
        number=${WIDGET//[^[:digit:]]/}
        replace-pacman-command "-"$pacman_bindings[$number+1]
    }

    # Create a widet and bind it to the key
    zle -N "replace-pacman-command-"$i
    bindkey $pacman_bindings[$i] "replace-pacman-command-"$i

    # We do this, because we don't have 2-dimensional arrays
    i=$((i+2))
done

# IRC client like history
# http://zshwiki.org/home/zle/ircclientlikeinput
fake-accept-line() {
  if [[ -n "$BUFFER" ]];
  then
    print -S "$BUFFER"
  fi
  return 0
}
zle -N fake-accept-line

down-or-fake-accept-line() {
  if (( HISTNO == HISTCMD )) && [[ "$RBUFFER" != *$'\n'* ]];
  then
    zle fake-accept-line
  fi
  zle .down-line-or-history "$@"
}

zle -N down-line-or-history down-or-fake-accept-line
