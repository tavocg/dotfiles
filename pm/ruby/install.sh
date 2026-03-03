#!/bin/sh

list="./list.sh"

set -ex

for gem in $PACKAGES; do
  gem install --user-install "$gem"
done
