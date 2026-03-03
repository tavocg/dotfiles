#!/bin/sh

if ! command -v npm >/dev/null 2>&1; then
  exit 0
fi

set -ex

npm install -g npm@latest

npm outdated -g --depth=0 | awk "NR>1 {print \$1}" | while IFS= read -r pkg; do
  if [ -n "$pkg" ]; then
    npm install -g "$pkg@latest"
  fi
done
