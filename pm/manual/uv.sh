#!/bin/sh
set -eu

REPO="astral-sh/uv"
DESTINATION="$HOME/.local/bin"
PATTERN='uv-x86_64-unknown-linux-gnu\.tar\.gz'

URL="$(curl -fsSL "https://api.github.com/repos/$REPO/releases/latest" | sed "/browser_download_url/!d;/$PATTERN/!d" | head -n 1)"
URL="${URL%\"*}"
URL="${URL##*\"}"

if [ -z "$URL" ]; then
  echo "Could not find matching asset in latest release"
  exit 1
fi

set -x
curl -fLsS "$URL" | tar -xz -C "$DESTINATION" --strip-components=1 "${PATTERN%%\\*}/uv" "${PATTERN%%\\*}/uvx"
set +x
