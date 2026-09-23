#!/bin/sh

if command -v monerod >/dev/null 2>&1; then
  mkdir -p "$XDG_DATA_HOME"/bitmonero
  alias monerod="monerod --data-dir $XDG_DATA_HOME/bitmonero"
fi
