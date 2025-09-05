#!/bin/sh
PKG_LIST="${PKG_LIST:-$XDG_CONFIG_HOME/packages/system/apt/list}"

if ! [ -f "$PKG_LIST" ]; then
	echo "No package list found at PKG_LIST=$PKG_LIST"
	exit 1
fi

pkgs="$(while read l; do
	pkg="${l%%#*}"
	if [ "$pkg" != "" ]; then
		printf '%s ' "$pkg"
	fi
done < "$PKG_LIST")"

sudo apt install -y $pkgs
