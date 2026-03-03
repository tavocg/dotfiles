#!/bin/sh

if ! command -v rustup >/dev/null 2>&1; then
  exit 0
fi

if ! command -v cargo >/dev/null 2>&1; then
  exit 0
fi

tc="$(rustup show active-toolchain)"
if [ -z "$tc" ]; then
  rustup default stable
fi

list="./list.sh"

. "$list"

set -ex
for pkg in $PACKAGES; do
  case "$pkg" in
  *https://*) cargo install --git "$pkg" --locked ;;
  *) cargo install "$pkg" --locked ;;
  esac
done
