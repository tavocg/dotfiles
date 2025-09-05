#!/bin/sh

export NVM_DIR="$HOME/.config/nvm"

if [ -s "$NVM_DIR/nvm.sh" ]; then
  # nvm.sh is a very large file that causes shell startup to slow
  # down, this wrapper sources nvm.sh only after calling nvm.
  nvm() {
    . "$NVM_DIR/nvm.sh"
    nvm "$@"
  }
fi
