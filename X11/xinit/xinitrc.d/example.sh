#!/bin/sh
#
# Example initialization script.
#
# In order to run commands on start, create scripts such
# as this one in the .config/X11/init.d directory and make
# them executable.

# Keyboard configuration
#xset r rate 300 70
#setxkbmap en

# Monitors
#xrandr --output DVI-D-0 --right-of HDMI-0

# Appearance
#picom --experimental-backends --animation-for-open-window=zoom &
#xsetroot -solid '#000000'

# Startup programs
#STATUS_BLOCKS="tray vol mic net bat kbd time date" dwmstatus & # Status bar
#emacs --daemon &
#clipse -listen &
#dunst
