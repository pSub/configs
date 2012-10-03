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

# Set DPMS off
xset dpms off &

# Set keyboard layout
setxkbmap -layout de -variant nodeadkeys &

# Load the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Load touchpad (synaptics) configuration
$HOME/.bin/synclient-config &

# Vertical and horizontal scrolling on Thinkpads
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5

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

# Start xmodmap before dwm to make sure the changes
# from xmodmap are applied
xmodmap $HOME/.Xmodmap && exec dwm
