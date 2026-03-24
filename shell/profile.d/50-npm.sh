# shellcheck shell=sh

case ":$PATH:" in
*":${XDG_DATA_HOME:-$HOME/.local/share}/npm/bin:"*) ;;
*) export PATH="${XDG_DATA_HOME:-$HOME/.local/share}/npm/bin${PATH:+:${PATH}}" ;;
esac
