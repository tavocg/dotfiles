#!/bin/sh
REPO="pythops/impala"
TARGET="impala"
DESTINATION="/usr/local/bin"
PATTERN="-x86_64-unknown-linux-musl"

URL="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | sed "/browser_download_url/!d;/$PATTERN/!d" | head -n 1)"
URL="${URL%\"*}"
URL="${URL##*\"}"

if [ -z "$URL" ]; then
  echo "Could not find matching asset in latest release"
  exit 1
fi

set -ex
sudo curl -fLsS "$URL" -o "$DESTINATION"/"$TARGET"
sudo chmod +x "$DESTINATION"/"$TARGET"
