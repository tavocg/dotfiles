#!/bin/sh

if command -v lxpolkit >/dev/null; then
  lxpolkit &
else
  echo "error: lxpolkit not found"
fi
