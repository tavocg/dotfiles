#!/bin/sh

f_print_front_matter() {
  file="$1"

  if [ -z "$file" ] || ! [ -f "$file" ]; then
    echo "File '$file' not found"
    return 1
  fi

  if command -v batcat >/dev/null 2>&1; then
    PAGER="batcat -p -l sh"
  else
    PAGER=less
  fi

  awk '$0 ~ /#!/ {next} $0 !~ /#/ {exit} {line = $0; sub(/^#[[:space:]]*/, "", line); print line}' "$file" | $PAGER
}
