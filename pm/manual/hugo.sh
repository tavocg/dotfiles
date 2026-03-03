#!/bin/sh
set -eu

REPO="gohugoio/hugo"
TARGET="hugo"
DESTINATION="/usr/local/bin"
PATTERN='_linux-amd64\.tar\.gz'

URL="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | sed "/browser_download_url/!d;/$PATTERN/!d" | head -n 1)"
URL="${URL%\"*}"
URL="${URL##*\"}"

if [ -z "$URL" ]; then
  echo "Could not find matching asset in latest release"
  exit 1
fi

set -x
curl -fLsS "$URL" | sudo tar -xz -C "$DESTINATION" "$TARGET"
set +x
