# shellcheck shell=sh

case ":$XCURSOR_PATH:" in
*":${XDG_DATA_HOME:-$HOME/.local/share}/icons:"*) ;;
*) export XCURSOR_PATH="${XDG_DATA_HOME:-$HOME/.local/share}/icons${XCURSOR_PATH:+:${XCURSOR_PATH}}" ;;
esac

case ":$XCURSOR_PATH:" in
*":/run/current-system/sw/share/icons:"*) ;;
*) export XCURSOR_PATH="/run/current-system/sw/share/icons${XCURSOR_PATH:+:${XCURSOR_PATH}}" ;;
esac

case ":$XCURSOR_PATH:" in
*":/nix/var/nix/profiles/default/share/icons:"*) ;;
*) export XCURSOR_PATH="/nix/var/nix/profiles/default/share/icons${XCURSOR_PATH:+:${XCURSOR_PATH}}" ;;
esac
