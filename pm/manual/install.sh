#!/bin/sh

for script in *; do
  if [ -x "$script" ] && [ "$script" != "install.sh" ]; then
    . "./$script"
  fi
done
