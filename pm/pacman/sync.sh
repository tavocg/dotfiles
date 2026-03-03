#!/bin/sh

if ! command -v pacman >/dev/null 2>&1; then
  exit 0
fi

set -ex
sudo pacman -Sy
