#!/bin/sh

if command -v clipse >/dev/null; then
  clipse -listen-shell &
else
  echo "error: clipse not found"
fi
