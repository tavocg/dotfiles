#!/bin/sh
# See: https://wiki.archlinux.org/title/Xsettingsd

if command -v xsettingsd >/dev/null 2>&1; then
  xsettingsd -c "${XDG_CONFIG_HOME:-$HOME/.config}"/X11/xsettingsd.conf &
fi
