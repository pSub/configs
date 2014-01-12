#!/bin/sh

# Displays the battery status using the acpi tool.
batmon(){
    acpi -b | awk 'gsub(/,/,"") {print $3 $4}' | sed 's/Charging/↑/;s/Discharging/↓/;s/Full/•/'
}

# Load xrandr configuration
$HOME/bin/xrandr-config

# Start urxvt daemon
urxvtd -q -f -o

# Start notification daemon
dunst &

# Start daemon for plug'n play
udisks-glue -c $HOME/.udisks-glue.conf &

# Disable beep
xset b off &

# Set DPMS off
xset -dpms &

# Load the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Load touchpad (synaptics) configuration
$HOME/bin/synclient-config &

# Vertical and horizontal scrolling on Thinkpads
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5

# Set background / wallpaper
$HOME/bin/wallpaper &

# Autostart applications
$HOME/bin/x-autostart &

# Start the screen locker before xmodmap
# to work around this issue:
# http://bugs.i3wm.org/report/ticket/1100
$HOME/bin/lock && xmodmap $HOME/.Xmodmap
