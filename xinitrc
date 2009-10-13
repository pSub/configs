volume(){
    state="`amixer get Speaker |awk '/Front\ Left:/ {print $4}'`" 
    if test "$state" != "[on]"; then
        echo "Mute"
    else
        echo "`amixer get PCM | awk '/Front\ Left:/ {print $5}' | sed -e 's/\[//g'| sed -e 's/%\]//g'`"
    fi
}

xbindkeys -f ~/.xbindkeysrc
synclient TouchpadOff=1
bgs $HOME/wallpapers/Treehugger-1400x1050.jpg
while xsetroot -name "[Vol: `volume`]   `batmon`  [`date +"%a %d.%m.%Y  %H:%M"`]"; do
       sleep 1
done &
exec dwm 2> ~/log/dwm.log
