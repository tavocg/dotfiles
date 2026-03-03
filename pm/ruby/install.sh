#!/bin/sh

if ! command -v gem >/dev/null 2>&1; then
  return 0
fi

if ! command -v ruby >/dev/null 2>&1; then
  return 0
fi

list="./list.sh"

set -ex

for gem in $PACKAGES; do
  gem install --user-install "$gem"
done
