# Display the speaker/headphone volume
volume(){
    speaker="`amixer get Speaker | awk '/Front\ Left:/ {print $7}'`" 
    if test "$speaker" != "[on]"; then
        echo "Mute"
     else
        echo "`amixer get Master | awk '/Mono:/ {print $4}' | sed -e 's/\[//g'| sed -e 's/%\]//g'`"
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

addprefix(){
  for s in $(cat $2)
  do
    echo -n $1$s" "
  done
}

exec $HOME/.bin/xrandr &

# Disable beep
xset b off

# Set keyboard layout
setxkbmap -layout de

# Loading the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Disable touchpad clicks while typing
#syndaemon -d
synclient VertEdgeScroll=1

# Disable touchpad tap click
synclient TapButton1=0
synclient TapButton2=0
synclient TapButton3=0

# Set wm name (useful for java apps)
wmname LG3D &

BGPATH=$HOME/wallpapers/
BGACTIVE=$BGPATH/current

# Set background
bgs $(addprefix $BGPATH $BGACTIVE)

# Start urxvt daemon
urxvtd -q -f -o

urxvtc -e /home/pascal/.bin/checkforsync & 
#thunderbird &
#firefox &

# Refresh the dwm statusbar every second
while true; do
      xsetroot -name "[Vol: `volume`] [Bat: `batmon`] [`date +'%a %d.%m.%Y  %H:%M'`]";
      sleep 1
done &

# Uncomment your favor wm
xmodmap $HOME/.Xmodmap && exec dwm
#exec xmonad
