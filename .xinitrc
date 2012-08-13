#!/bin/sh
# Display wifi signal strength
wifi(){
    state="`iwconfig wlan0 | grep -oE '([0-9]+)/([0-9]+)'`"
    if test "$state" == ""; then
        echo "D/C"
    else
        echo $state
    fi
}

batmon(){
    acpi -b | awk 'gsub(/,/,"") {print $3 $4}' | sed 's/Charging/↑/;s/Discharging/↓/;s/Full/•/'
}

# Load xrandr configuration
$HOME/.bin/xrandr-config

# Start urxvt daemon
urxvtd -q -f -o

# Start notification daemon
dunst &

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

# Start scdaemon
scdaemon --daemon

# Start gpg-agent
eval "$(gpg-agent --daemon --enable-ssh-support)"

# Set background / wallpaper
$HOME/.bin/wallpaper &

# Autostart applications
$HOME/.bin/x-autostart &

# Refresh the dwm statusbar every second
while xsetroot -name "[Bat: `batmon`] [`date +'%a %d.%m.%Y  %H:%M'`]"; do
      sleep 1
done &

# Start xmodmap with dwm to make sure changes
# form xmodmap are applied
xmodmap $HOME/.Xmodmap && exec dwm
