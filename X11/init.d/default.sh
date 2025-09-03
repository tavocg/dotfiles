#!/bin/sh
# Default initialization script

# Environment for dbus and xdg-desktop-portal
dbus-update-activation-environment --systemd --all
systemctl --user import-environment DISPLAY
lxpolkit &

# A bit of grace time to press Ctrl+C if needed
sleep 0.2
