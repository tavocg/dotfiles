# shellcheck shell=sh

export MBSYNCRC="${XDG_CONFIG_HOME:-$HOME/.config}/isync/mbsyncrc"
alias mbsync="mbsync -c "$XDG_CONFIG_HOME"/isync/mbsyncrc"
