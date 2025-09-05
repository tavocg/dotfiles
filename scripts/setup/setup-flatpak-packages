#!/bin/sh
EXPORTS_BIN_PREFIX="$HOME/.local/bin"
FLATPAK_BIN_PREFIX="/var/lib/flatpak/exports/bin"
FLATPAK_LIST="$XDG_CONFIG_HOME"/packages/flatpak/list.yml

while read line; do
	bin="${line%%:*}"
	app="${line##*:}"
	app="${app##* }"
	ln -sf "$FLATPAK_BIN_PREFIX"/"$app" "$EXPORTS_BIN_PREFIX"/"$bin"
	app_list="$app_list $app"
done < "$FLATPAK_LIST"

flatpak install -y $app_list
