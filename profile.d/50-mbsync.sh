# shellcheck shell=sh

export MBSYNC_MAILDIR="${MBSYNC_MAILDIR-$HOME/.local/share/isync}"

if ! [ -d "$MBSYNC_MAILDIR" ]; then
  mkdir -p "$MBSYNC_MAILDIR"
fi

export MBSYNCRC="${XDG_CONFIG_HOME:-$HOME/.config}/isync/mbsyncrc"
alias mbsync="mbsync -c "$XDG_CONFIG_HOME"/isync/mbsyncrc"
