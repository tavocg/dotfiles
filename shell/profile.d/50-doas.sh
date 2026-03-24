# shellcheck shell=sh

if [ -n "$BASH_VERSION" ] && command -v doas >/dev/null 2>&1; then
  complete -cf doas
fi
