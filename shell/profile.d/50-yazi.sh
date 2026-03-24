# shellcheck shell=sh

y() {
  tmp="$(mktemp -t "yazi-cwd.XXXXXX")"
  yazi "$@" --cwd-file="$tmp"

  cwd=""
  while IFS= read -r line; do
    cwd="$cwd$line"
  done <"$tmp"

  [ -n "$cwd" ] && [ "$cwd" != "$PWD" ] && builtin cd -- "$cwd" || return
  rm -f -- "$tmp"
}
