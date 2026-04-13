# shellcheck shell=sh

# See: https://yazi-rs.github.io/docs/quick-start#shell-wrapper
if command -v yazi >/dev/null 2>&1; then
  y() {
    set -- "$@" --cwd-file "$(mktemp -t yazi-cwd.XXXXXX)"
    command yazi "$@"
    shift $(($# - 1))
    set -- "$(
      command cat <"$1"
      printf .
      rm -f -- "$1"
    )"
    set -- "${1%.}"
    [ "$1" != "$PWD" ] && [ -d "$1" ] && command cd -- "$1"
  }
fi
