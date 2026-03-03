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

for package_manager in *; do
  _info "starting sync..."
  if [ -d "$package_manager" ]; then
    if [ -f "$package_manager/pre-sync.sh" ]; then
      if ! (
        cd "$package_manager" || exit 1
        sh ./pre-sync.sh
      ) >>"$LOGS" 2>&1; then
        _warn "$package_manager is not available"
        continue
      fi
    elif [ -f "$package_manager/pre-update.sh" ]; then
      if ! (
        cd "$package_manager" || exit 1
        sh ./pre-update.sh
      ) >>"$LOGS" 2>&1; then
        _warn "$package_manager is not available"
        continue
      fi
    elif [ -f "$package_manager/pre-install.sh" ]; then
      if ! (
        cd "$package_manager" || exit 1
        sh ./pre-install.sh
      ) >>"$LOGS" 2>&1; then
        _warn "$package_manager is not available"
        continue
      fi
    fi
    if [ -f "$package_manager/sync.sh" ]; then
      _info "syncing packages for $package_manager..."
      if ! (
        cd "$package_manager" || exit 1
        sh ./sync.sh
      ) >>"$LOGS" 2>&1; then
        _error "error syncing packages for $package_manager, see logs in $LOGS" >&2
        exit 1
      fi
    fi
  fi
done

_info "done!"
