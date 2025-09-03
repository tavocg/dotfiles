#!/bin/sh
# Default initialization script
# These commands are required at startup,
# If removed, you might run into errors.

# Environment for dbus and xdg-desktop-portal
dbus-update-activation-environment --systemd --all
systemctl --user import-environment DISPLAY

# Policy kit
lxpolkit &

# A bit of grace time to press Ctrl+C if needed
sleep 0.2
