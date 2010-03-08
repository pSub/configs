volume(){
    state="`amixer get Speaker |awk '/Front\ Left:/ {print $4}'`" 
    if test "$state" != "[on]"; then
        echo "Mute"
    else
        echo "`amixer get PCM | awk '/Front\ Left:/ {print $5}' | sed -e 's/\[//g'| sed -e 's/%\]//g'`"
    fi
}
wifi(){
    state="`iwconfig wlan0 | sed 's/ /\n/g' | grep -o '[0-9]*[0-9]/[0-9]*[0-9]'`"
    if test "$state" == ""; then
        echo "D/C"
    else
        echo $state
    fi
}

setxkbmap -layout de-latin1-nodeadkeys & 
xbindkeys -f $HOME/.xbindkeysrc &
xmodmap $HOME/.xmodmaprc &
synclient TouchpadOff=1 &
wmname LG3D &
bgs $HOME/wallpapers/Treehugger-1400x1050.jpg &
thunderbird &
firefox &
while xsetroot -name "[Wifi: `wifi`] [Vol: `volume`] [Bat: `batmon`] [`date +"%a %d.%m.%Y  %H:%M"`]"; do
       sleep 1
done &
exec dwm 2> $HOME/.log/dwm.log
