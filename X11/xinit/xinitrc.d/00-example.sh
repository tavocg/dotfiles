#!/bin/sh
#
# Example initialization script.
#
# In order to run commands on start, create scripts such
# as this one in the .config/X11/xinit/xinitrc.d directory and make
# them executable.

# English keymap
#setxkbmap en

# Monitors
#xrandr --output DVI-D-0 --right-of HDMI-0

# Appearance
#picom --experimental-backends --animation-for-open-window=zoom &
#xsetroot -solid '#000000'

# Startup programs
#STATUS_BLOCKS="tray vol mic net bat kbd time date" dwmstatus & # Status bar
#emacs --daemon &
#dunst
