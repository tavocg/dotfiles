#!/bin/sh

export NVM_DIR="$HOME/.config/nvm"

_nvm_lazy_load() {
  unset -f nvm npm npx node

  if [ -s "$NVM_DIR/nvm.sh" ]; then
    . "$NVM_DIR/nvm.sh"
  fi
}

nvm() {
  _nvm_lazy_load
  nvm $@
}

npm() {
  _nvm_lazy_load
  npm $@
}

npx() {
  _nvm_lazy_load
  npx $@
}

node() {
  _nvm_lazy_load
  node $@
}
