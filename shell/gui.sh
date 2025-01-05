#!/bin/sh

if [ "$(tty)" = "/dev/tty1" ] ; then
    sleep 0.5
    amixer &
    XDG_SESSION_TYPE=x11 GDK_BACKEND=x11 exec startx
    #exec sway
elif [ "$(tty)" = "/dev/tty2" ] ; then
    sleep 0.5
    exec /usr/lib/plasma-dbus-run-session-if-needed /usr/bin/startplasma-wayland
fi
