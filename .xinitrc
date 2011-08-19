#!/bin/sh
# Display the speaker/headphone volume
volume(){
    speaker="`amixer get Master | awk '/Mono:/ {print $6}'`" 
    if test "$speaker" != "[on]"; then
        echo "Mute"
     else
        echo "`amixer get Master | awk '/Mono:/ {print $4}' | sed -e 's/\[\|%\]//g'`"
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

# Adds first parameter as prefix to the lines of the second parameter
# therefore the second parameter must bei a file
addprefix(){
  for s in $(cat $2)
  do
    echo -n $1$s" "
  done
}

# Path to background images
BGPATH=$HOME/wallpapers/

# Path to file with active backgrounds
# one line per background
BGACTIVE=$BGPATH/current

# Load xrandr configuration
$HOME/.bin/xrandr-config

# Start urxvt daemon
urxvtd -q -f -o

# Start notification daemon
/usr/lib/notification-daemon-1.0/notification-daemon &

# Start daemon for plug'n play
udisks-glue -c $HOME/.udisks-glue.conf &

# Disable beep
xset b off &

# Set keyboard layout
setxkbmap -layout de -variant nodeadkeys &

# Load the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Load touchpad (synaptics) configuration
$HOME/.bin/synclient-config &

# Set window-manager name (useful for java apps)
wmname LG3D &

# Set background
bgs $(addprefix $BGPATH $BGACTIVE)

# Start scdaemon
scdaemon --daemon

# Start gpg-agent
eval "$(gpg-agent --daemon --enable-ssh-support)"

# Autostart applications
$HOME/.bin/x-autostart &

# Refresh the dwm statusbar every second
while true; do
      xsetroot -name "[Fin: `finances -q show`] [Vol: `volume`] [Bat: `batmon`] [`date +'%a %d.%m.%Y  %H:%M'`]";
      sleep 1
done &

# Start xmodmap with dwm to make sure changes
# form xmodmap are applied
xmodmap $HOME/.Xmodmap && exec dwm
