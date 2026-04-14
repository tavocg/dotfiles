#!/bin/sh

if ! [ -d "${XDG_CONFIG_HOME:-$HOME/.config}"/jupyter ]; then
  mkdir -p "${XDG_CONFIG_HOME:-$HOME/.config}"/jupyter
fi
