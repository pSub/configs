# Display the speaker/headphone volume
volume(){
    speaker="`amixer get Speaker | awk '/Front\ Left:/ {print $4}'`" 
    headphone="`amixer get Headphone | awk '/Front\ Left:/ {print $4}'`"
    if test "$speaker" != "[on]" && test "$headphone" != "[on]"; then
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

# Disable beep
xset b off

# Loading the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Loading the xmodmap configuration (capslock -> escape)
xmodmap /home/pascal/.xmodmaprc &

# Preloading dmenu cache
dmenu_path > /dev/null

# urxvt daemon
urxvtd &

# Disable touchpad
synclient TouchpadOff=1 &

# Set wm name (useful for java apps)
wmname LG3D &

# Set background
bgs $HOME/wallpapers/Dystopia-2000x1333.jpg &
thunderbird &
firefox &

# Refresh the dwm statusbar every second
while xsetroot -name "[Wifi: `wifi`] [Vol: `volume`] [Bat: `batmon`] [`date +"%a %d.%m.%Y  %H:%M"`]"; do
      sleep 1
done &

# Uncomment your favor wm
exec dwm
#exec xmonad
