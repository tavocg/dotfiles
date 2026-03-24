#!/bin/sh
REPO="sxyazi/yazi"
TARGET="yazi"
DESTINATION="$HOME/.local/bin"
PATTERN='yazi-x86_64-unknown-linux-gnu\.zip'

die() {
  if [ -d "$1" ]; then
    rm -rf "$1"
  fi
  exit 1
}

URL="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | sed "/browser_download_url/!d;/$PATTERN/!d" | head -n 1)"
URL="${URL%\"*}"
URL="${URL##*\"}"

if [ -z "$URL" ]; then
  echo "Could not find matching asset in latest release"
  exit 1
fi

tmp="$(mktemp -d)" || die

set -x
(
  cd "$tmp"
  curl -fLsS "$URL" -o "$TARGET".zip
  unzip -qj "$TARGET".zip

  mv ya "$DESTINATION"/
  chmod +x "$DESTINATION"/ya
  mv yazi "$DESTINATION"/
  chmod +x "$DESTINATION"/yazi
) || die "$tmp"
set +x
