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
    state="`iwconfig wlan0 | grep -oE '([0-9]+)/([0-9]+)'`"
    if test "$state" == ""; then
        echo "D/C"
    else
        echo $state
    fi
}

# Load xrandr
exec /home/pascal/.bin/xrandr &

# urxvt daemon
urxvtd &

# Disable touchpad clicks while typing
#syndaemon -d

# Disable touchpad
synclient TouchpadOff=1 &

# Disable beep
xset b off

# Loading the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Loading the xmodmap configuration (capslock -> escape)
setxkbmap -option "caps:escape"
setxkbmap -option "compose:lwin"

# Using fuse to make dmenu faster
# no more dmenu_path needed
unionfs $PATH ~/.path

# Set wm name (useful for java apps)
wmname LG3D &

# Add all programms which depend on
# synchronised data in this script
urxvtc -e /home/pascal/.bin/checkforsync

# Autostart without synchronisation
thunderbird &
firefox &

# Set background
bgs $HOME/wallpapers/Blue_Moon-1920x1200.jpg

# Refresh the dwm statusbar every second
while true; do
    xsetroot -name "[Volume: `volume`] [Wifi: `wifi`] [Bat: `batmon`] [`date +'%a %d.%m.%Y  %H:%M'`]";
    sleep 1
done &

# Uncomment your favor wm
exec dwm
#exec xmonad
