#!/bin/sh

if ! command -v flatpak >/dev/null 2>&1; then
  exit 0
fi

set -ex
flatpak update -y
