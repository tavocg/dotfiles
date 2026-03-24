# shellcheck shell=sh

case ":$PATH:" in
*":${XDG_CONFIG_HOME:-$HOME/.config}/emacs/bin:"*) ;;
*) export PATH="${XDG_CONFIG_HOME:-$HOME/.config}/emacs/bin${PATH:+:${PATH}}" ;;
esac
