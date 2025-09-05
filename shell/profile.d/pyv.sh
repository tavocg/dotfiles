#!/usr/bin/env bash

for f in pyv pyv_comp.bash; do
  if [ -f ~/.local/share/pyv/$f ]; then
    # shellcheck disable=SC1090
    . ~/.local/share/pyv/$f
  fi
done
