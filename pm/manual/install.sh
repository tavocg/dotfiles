#!/bin/sh

for script in *; do
  if [ -x "$script" ]; then
    . "./$script"
  fi
done
