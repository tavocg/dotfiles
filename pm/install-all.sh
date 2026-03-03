#!/bin/sh
LOGS="$XDG_STATE_HOME"/pm/install.log

_info() {
  printf '\e[2m[%s]\e[0m \e[32m[%s]\e[0m %s\n' "pm" "INFO" "$1"
}

_error() {
  printf '\e[2m[%s]\e[0m \e[31m[%s]\e[0m %s\n' "pm" "ERROR" "$1"
}

if ! [ -d "${LOGS%/*}" ]; then
  mkdir -p "${LOGS%/*}"
fi

for package_manager in *; do
  if [ -d "$package_manager" ]; then
    (
      cd "$package_manager"
      if [ -f "./install.sh" ]; then
        _info "installing packages for $package_manager..."
        if ! . "./install.sh" >>"$LOGS" 2>&1; then
          _error "error installing packages for $package_manager, see logs in $LOGS"
          exit 1
        fi
      fi
    )
  fi
done
