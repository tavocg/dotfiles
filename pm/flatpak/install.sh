#!/bin/sh
FLATPAK_EXPORTS="${FLATPAK_EXPORTS:-$HOME/.local/bin}"
FLATPAK_BIN_PREFIX="${FLATPAK_BIN_PREFIX:-/var/lib/flatpak/exports/bin}"
list="./list.sh"

if ! command -v flatpak >/dev/null 2>&1; then
  exit 0
fi

if ! [ -d "$FLATPAK_EXPORTS" ]; then
  mkdir -p "$FLATPAK_EXPORTS"
fi

. "$list"

for pkg in $PACKAGES; do
  bin="${pkg%%:*}"
  name="${pkg##*:}"

  if [ -n "$bin" ]; then
    set -ex
    ln -sf "$FLATPAK_BIN_PREFIX"/"$name" "$FLATPAK_EXPORTS"/"$bin"
    set +ex
  fi

  if [ -n "$pkgs" ]; then
    pkgs="$pkgs $name"
  else
    pkgs="$name"
  fi
done

set -ex
flatpak install -y $pkgs
