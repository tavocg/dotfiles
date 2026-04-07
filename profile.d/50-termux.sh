# shellcheck shell=sh

if [ -d /data/data/com.termux ]; then
  export \
    IS_TERMUX=1 \
    TERM=xterm-256color \
    EDITOR=nvim
fi
