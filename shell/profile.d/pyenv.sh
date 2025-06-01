#!/bin/bash

# export PYENV_ROOT="$HOME/.pyenv" # Already in xdgspec.sh
[ -d $PYENV_ROOT/bin ] && export PATH="$PYENV_ROOT/bin:$PATH"
# export PATH="$HOME/.pyenv/shims:${PATH}" # Already set in path.sh
export PYENV_SHELL=bash
command pyenv rehash 2>/dev/null

pyenv() {
    local command=${1:-}
    [ "$#" -gt 0 ] && shift
    case "$command" in
        rehash|shell) eval "$(pyenv "sh-$command" "$@")";;
        *) command pyenv "$command" "$@";;
    esac
}
