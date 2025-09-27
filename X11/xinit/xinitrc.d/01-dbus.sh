#!/bin/sh
# updates the list of environment variables used by dbus

dbus-update-activation-environment --systemd --all
systemctl --user import-environment DISPLAY
