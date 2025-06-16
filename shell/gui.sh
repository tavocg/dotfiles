#!/bin/sh

if [ "$(tty)" = "/dev/tty1" ] ; then
    ## KDE WAYLAND
    #exec /usr/lib/plasma-dbus-run-session-if-needed /usr/bin/startplasma-wayland

    ## DWM
    #sleep 0.5
    #amixer &
    #XDG_SESSION_TYPE=x11 GDK_BACKEND=x11 exec startx

    ## SWAY
    exec sway
fi
