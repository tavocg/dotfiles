#!/bin/sh
LOGS="$XDG_STATE_HOME"/pm/install.log

_info() {
  printf '\e[2m[%s]\e[0m \e[34m[%s]\e[0m %s\n' "pm" "INFO" "$1"
}

_warn() {
  printf '\e[2m[%s]\e[0m \e[33m[%s]\e[0m %s\n' "pm" "WARN" "$1"
}

_error() {
  printf '\e[2m[%s]\e[0m \e[31m[%s]\e[0m %s\n' "pm" "ERROR" "$1"
}

if ! [ -d "${LOGS%/*}" ]; then
  mkdir -p "${LOGS%/*}"
fi

_info "starting update..."

for package_manager in *; do
  if [ -d "$package_manager" ]; then
    if [ -f "$package_manager/pre-update.sh" ]; then
      if ! (
        cd "$package_manager" || exit 1
        sh ./pre-update.sh
      ) >>"$LOGS" 2>&1; then
        _warn "$package_manager is supported but not available, skipping"
        continue
      fi
    fi
    if [ -f "$package_manager/update.sh" ]; then
      _info "updating packages for $package_manager..."
      if ! (
        cd "$package_manager" || exit 1
        sh ./update.sh
      ) >>"$LOGS" 2>&1; then
        _error "error updating packages for $package_manager, see logs in $LOGS" >&2
        exit 1
      fi
    fi
  fi
done

_info "done!"
