# shellcheck shell=sh

export PATH="${XDG_STATE_HOME:-$HOME/.local/state}/nix/profile/bin${PATH:+:${PATH}}"
