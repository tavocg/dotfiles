#!/bin/sh
LOGS="$XDG_STATE_HOME"/pm/install.log

_info() {
  printf '\e[2m[%s]\e[0m \e[34m[%s]\e[0m %s\n' "pm" "INFO" "$1"
}

_error() {
  printf '\e[2m[%s]\e[0m \e[31m[%s]\e[0m %s\n' "pm" "ERROR" "$1"
}

if ! [ -d "${LOGS%/*}" ]; then
  mkdir -p "${LOGS%/*}"
fi

for package_manager in *; do
  if [ -d "$package_manager" ]; then
    if [ -f "$package_manager/install.sh" ]; then
      _info "installing packages for $package_manager..."
      if ! (
        cd "$package_manager" || exit 1
        sh ./install.sh
      ) >>"$LOGS" 2>&1; then
        _error "error installing packages for $package_manager, see logs in $LOGS" >&2
        exit 1
      fi
    fi
  fi
done

_info "done!"
