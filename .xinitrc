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

# Set keyboard layout if not on NixOS
# Temporarily on all systems because of http://bugs.i3wm.org/report/ticket/1100
#if [[ $(uname -a) != *"NixOS"* ]]; then
  setxkbmap -layout de -variant nodeadkeys &
#fi

# Load the xbindkeys configuration
xbindkeys -f $HOME/.xbindkeysrc &

# Load touchpad (synaptics) configuration
$HOME/bin/synclient-config &

# Vertical and horizontal scrolling on Thinkpads
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation" 8 1
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Button" 8 2
xinput set-int-prop "TPPS/2 IBM TrackPoint" "Evdev Wheel Emulation Axes" 8 6 7 4 5

# Set window-manager name (useful for java apps)
wmname LG3D &

# Start gpg-agent if not on NixOS
if [[ $(uname -a) != *"NixOS"* ]]; then
  eval "$(gpg-agent --daemon --enable-ssh-support)"
fi

# Set background / wallpaper
$HOME/bin/wallpaper &

# Autostart applications
$HOME/bin/x-autostart &

# Refresh the dwm statusbar every second
while true; do
      xsetroot -name "[Bat: `batmon`] [`date +'%a %d.%m.%Y  %H:%M'`]"
      sleep 1
done &

# Lock the screen after startup.
$HOME/bin/lock

# Start xmodmap before dwm to make sure the changes
# from xmodmap are applied
xmodmap $HOME/.Xmodmap && exec dwm
