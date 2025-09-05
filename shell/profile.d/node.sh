#!/bin/sh

export NODE_REPL_HISTORY="$HOME"/.local/state/node_repl_history

if ! [ -d "${NODE_REPL_HISTORY%/*}" ]; then
  mkdir -p "${NODE_REPL_HISTORY%/*}"
  touch "$NODE_REPL_HISTORY"
fi
