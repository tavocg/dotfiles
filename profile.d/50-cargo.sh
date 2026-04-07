# shellcheck shell=sh

case ":$PATH:" in
*":${XDG_DATA_HOME:-$HOME/.local/share}/cargo/bin:"*) ;;
*) export PATH="${XDG_DATA_HOME:-$HOME/.local/share}/cargo/bin${PATH:+:${PATH}}" ;;
esac
