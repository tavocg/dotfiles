# shellcheck shell=sh

export MBSYNC_MAILDIR="${MBSYNC_MAILDIR-$HOME/.local/share/isync}"

for maildir in "$MBSYNC_MAILDIR" "$MBSYNC_MAILDIR"/personal "$MBSYNC_MAILDIR"/ucr; do
  if ! [ -d "$maildir" ]; then
    mkdir -p "$maildir"
  fi
done

export MBSYNCRC="${XDG_CONFIG_HOME:-$HOME/.config}/isync/mbsyncrc"
alias mbsync="mbsync -c "$XDG_CONFIG_HOME"/isync/mbsyncrc"
