#!/bin/bash

# Not sure if this is necessary
#BASH_ENV="${XDG_CONFIG_HOME:-$HOME/.config/}/shell/bashrc"

# shellcheck disable=SC2043
for d in ~/.config/profile.d; do
  if [ -d "$d" ]; then
    for p in "$d"/*.sh; do
      if [ -r "$p" ]; then
        # shellcheck source=/dev/null
        . "$p"
      fi
    done
  fi
done

case $- in
*i*) ;;
*) return ;;
esac

_prompt_git_branch() {
  GIT_BRANCH="$(git branch 2>/dev/null | sed '/\*/!d;s/^\*\s*//g;s/\s*$//g')"
  [ -n "$GIT_BRANCH" ] && printf '%s ' "$GIT_BRANCH"
}

PROMPT_COMMAND='if [ "$?" = 0 ]; then EXIT_COLOR="\033[32m"; else EXIT_COLOR="\033[31m"; fi'
PS1='\[\033[2m\]\A\[\033[0m\] \[\033[34m\]\w\[\033[0m\] \[\033[35m\]\[\033[1m\]$(_prompt_git_branch)\[\033[0m\]\[$(echo -ne $EXIT_COLOR)\]>\[\033[0m\] '

bind "set completion-ignore-case on"
shopt -s checkwinsize
shopt -s histappend
shopt -s cdspell
shopt -s autocd
set -o vi

HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/bash_history"
HISTCONTROL=ignoreboth
# shellcheck disable=SC2034
HISTIZE=
HISTFILESIZE=

if [ -n "$HISTFILE" ] && ! [ -d "${HISTFILE%/*}" ]; then
  mkdir -p "${HISTFILE%/*}" && touch "$HISTFILE"
fi

if ! shopt -oq posix; then
  for comp in /usr/share/bash-completion/bash_completion /etc/bash_completion; do
    if [ -r "$comp" ]; then
      # shellcheck source=/dev/null
      . "$comp"
    fi
  done
fi

if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init posix --hook prompt)"
fi
