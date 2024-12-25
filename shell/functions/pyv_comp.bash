#!/usr/bin/env bash

_pyv_comp() {
    if [ -z "$VENV_DIR" ] ; then
        if [ -n "$XDG_DATA_HOME" ] ; then
            VENV_DIR="$XDG_DATA_HOME/pyv"
        else
            [ -z "$HOME" ] && return 1
            VENV_DIR="$HOME/.local/share/pyv"
        fi
    fi

    if [ "${#COMP_WORDS[@]}" == "2" ]; then
        COMPREPLY=($(compgen -W "ls new rm enter exit" "${COMP_WORDS[1]}"))
    fi

    if [ "${#COMP_WORDS[@]}" -gt 2 ]; then
        venvs="$(for v in "$VENV_DIR"/* ; do
            if [ -d "$v" ] ; then
                printf '%s ' "${v##*/}"
            fi
        done)"
        venvs="${venvs% *}"
        COMPREPLY=($(compgen -W "$venvs" -- "${COMP_WORDS[-1]}"))
    fi
}

complete -F _pyv_comp pyv
