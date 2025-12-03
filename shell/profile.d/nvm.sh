#!/bin/sh

export NVM_DIR="$HOME/.config/nvm"

_nvm_lazy_load() {
  if ! [ -f "package-lock.json" ] && ! [ -f "package.json" ] && ! [ -d "node_modules" ]; then
    return
  fi

  if command -v nvm >/dev/null 2>&1; then
    return
  fi

  if [ -s "$NVM_DIR/nvm.sh" ]; then
    . "$NVM_DIR/nvm.sh"
  fi
}
