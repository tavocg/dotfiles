#!/bin/sh

for gem in $PACKAGES; do
  gem install --user-install "$gem"
done
