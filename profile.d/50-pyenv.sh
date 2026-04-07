# shellcheck shell=sh

case ":$PATH:" in
*":${XDG_DATA_HOME:-$HOME/.local/share}/pyenv/shims:"*) ;;
*) export PATH="${XDG_DATA_HOME:-$HOME/.local/share}/pyenv/shims${PATH:+:${PATH}}" ;;
esac
