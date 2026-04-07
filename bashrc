#!/bin/bash
BASH_ENV="${XDG_CONFIG_HOME:-$HOME/.config/}/shell/bashrc"

for d in ~/.config/shell/profile.d; do
  if [ -d "$d" ]; then
    for s in "$d"/*.sh; do
      if [ -f "$s" ]; then
        . "$s"
      fi
    done
  fi
done

case $- in
  *i*) ;;
  *) return ;;
esac

HISTFILE="${XDG_STATE_HOME:-$HOME/.local/state}/shell/bash_history"
HISTCONTROL=ignoreboth
HISTIZE=
HISTFILESIZE=

if [ -n "$HISTFILE" ] && ! [ -d "${HISTFILE%/*}" ]; then
  mkdir -p "${HISTFILE%/*}" && touch "$HISTFILE"
fi

if ! shopt -oq posix; then
  [ -f /usr/share/bash-completion/bash_completion ] &&
    . /usr/share/bash-completion/bash_completion
  [ -f /etc/bash_completion ] &&
    . /etc/bash_completion
fi

bind "set completion-ignore-case on"
shopt -s checkwinsize
shopt -s histappend
shopt -s cdspell
shopt -s autocd
set -o vi

_prompt_git_branch() {
  GIT_BRANCH="$(git branch 2>/dev/null | sed '/\*/!d;s/^\*\s*//g;s/\s*$//g')"
  [ -n "$GIT_BRANCH" ] && printf '%s ' "$GIT_BRANCH"
}

PROMPT_COMMAND='if [ "$?" = 0 ]; then EXIT_COLOR="\033[32m"; else EXIT_COLOR="\033[31m"; fi'
PS1='\[\033[2m\]\A\[\033[0m\] \[\033[34m\]\w\[\033[0m\] \[\033[35m\]\[\033[1m\]$(_prompt_git_branch)\[\033[0m\]\[$(echo -ne $EXIT_COLOR)\]>\[\033[0m\] '

if command -v startdesktop >/dev/null 2>&1; then
  startdesktop
fi
