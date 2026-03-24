# shellcheck shell=sh

case ":$PATH:" in
*":${XDG_STATE_HOME:-$HOME/.local/state}/nix/profile/bin:"*) ;;
*) export PATH="${XDG_STATE_HOME:-$HOME/.local/state}/nix/profile/bin${PATH:+:${PATH}}" ;;
esac
