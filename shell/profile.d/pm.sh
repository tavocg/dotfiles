#!/bin/bash

_pm() {
  local cur prev opts

  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD - 1]}"

  opts="$(pm actions)"

  COMPREPLY=($(compgen -W "${opts}" -- "${cur}"))
  return 0
}

complete -F _pm pm
