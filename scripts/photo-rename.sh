#!/bin/sh

_rename() {
  for orig in *.JPG *.jpg *.NEF *.nef *.MOV *.mov; do
    if ! [ -f "$orig" ]; then
      continue
    fi

    ext="${orig##*.}"
    ext="$(echo "$ext" | tr '[:upper:]' '[:lower:]')"

    new_basename=""
    case "$ext" in
    jpg | nef)
      new_basename="$(exiftool -SubSecDateTimeOriginal "$orig" | sed 's/^.*: //;s/[: \.]//g')"
      ;;
    mov)
      new_basename="$(exiftool -DateTimeOriginal "$orig" | sed 's/^.*: //;s/[: \.]//g')00"
      ;;
    esac

    if [ -z "$new_basename" ]; then
      echo "error: file '$orig' could not be renamed."
      continue
    fi

    new="$new_basename"."$ext"

    if [ "$orig" != "$new" ]; then
      echo "mv '$orig' '$new'"
      mv "$orig" "$new"
    fi
  done
}

_rename
