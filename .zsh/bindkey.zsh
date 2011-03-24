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
