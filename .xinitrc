# Display the speaker volume
volume(){
    state="`amixer get Speaker |awk '/Front\ Left:/ {print $4}'`" 
    if test "$state" != "[on]"; then
        echo "Mute"
     else
        echo "`amixer get PCM | awk '/Front\ Left:/{print $5}' | sed -e 's/\[//g'| sed -e 's/%\]//g'`"
    fi
}

# Display wifi signal strength
wifi(){
    state="`iwconfig wlan0 | sed 's/ /\n/g' | grep -o '[000-999]/[000-999]'`"
    if test "$state" == ""; then
        echo "D/C"
    else
        echo $state
    fi
}

# Disable touchpad clicks while typing
#syndaemon -d

# Loading the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Loading the xmodmap configuration (capslock -> escape)
xmodmap /home/pascal/.xmodmaprc &

# Disable touchpad
synclient TouchpadOff=1 &

# Set wm name (useful for java apps)
wmname LG3D &

# Set background
bgs $HOME/wallpapers/Treehugger-1400x1050.jpg &
thunderbird &
firefox &

# Refresh the dwm statusbar every second
while xsetroot -name "[Wifi: `wifi`] [Vol: `volume`] [Bat: `batmon`] [`date +"%a %d.%m.%Y  %H:%M"`]"; do
      sleep 1
done &

# Uncomment your favor wm
exec dwm
#exec xmonad
