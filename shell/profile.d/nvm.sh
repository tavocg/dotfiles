#!/bin/sh

export NVM_DIR="$HOME/.config/nvm"

_nvm_cd_callback() {
  if ! [ -f "package-lock.json" ] && ! [ -f "package.json" ] && ! [ -d "node_modules" ]; then
    return
  fi

  [ -n "$_NVM_LOADED" ] && return

  if [ -s "$NVM_DIR/nvm.sh" ]; then
    _NVM_LOADED=1
    . "$NVM_DIR/nvm.sh"
  fi
}

_nvm_lazy_load() {
  [ -n "$_NVM_LOADED" ] && return

  if [ -s "$NVM_DIR/nvm.sh" ]; then
    . "$NVM_DIR/nvm.sh"
    _NVM_LOADED=1
  fi
}

nvm() {
  _nvm_lazy_load
  command nvm "$@"
}

node() {
  _nvm_lazy_load
  command node "$@"
}

npm() {
  _nvm_lazy_load
  command npm "$@"
}
