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

export \
  GL="git@gitlab.com:tavocg" \
  GH="git@github.com:tavocg" \
  DRIVE="ssh://drive:/home/drive/drive" \
  BOOKMARKS="$HOME/Documents/bookmarks" \
  BIB="$HOME/Documents/bibliography"

if ! command -v sudo >/dev/null 2>&1 && command -v doas >/dev/null 2>&1; then
  alias sudo="doas"
fi

alias \
  cfg='cd "$HOME"/.config/ && ls' \
  tmp='cd "$HOME"/Desktop/temp/ && ls' \
  dsk='cd "$HOME"/Desktop/ && ls' \
  prj='cd "$HOME"/Projects && ls' \
  doc='cd "$HOME"/Documents/ && ls' \
  dow='cd "$HOME"/Downloads/ && ls' \
  mus='cd "$HOME"/Music/ && ls' \
  prt='cd "$HOME"/Pictures/Screenshots/ && ls' \
  bkg='cd "$HOME"/Pictures/Backgrounds/ && ls' \
  img='cd "$HOME"/Pictures/ && ls' \
  vid='cd "$HOME"/Videos/ && ls'

if command -v exa >/dev/null 2>&1; then
  EZA_PROG="exa"
fi

if command -v eza >/dev/null 2>&1; then
  EZA_PROG="eza"
fi

if [ "$EZA_PROG" ]; then
  EZA_OPTS="--git --group-directories-first --icons --time-style=long-iso"
  alias la="$EZA_PROG $EZA_OPTS -alghUum"
  alias lt="$EZA_PROG $EZA_OPTS -T -L 2"
  alias ll="$EZA_PROG $EZA_OPTS -alg"
  alias l="$EZA_PROG $EZA_OPTS -alg"
  alias ls="$EZA_PROG $EZA_OPTS -1"
fi

if command -v trash >/dev/null 2>&1; then
  alias rm="trash"
fi

alias \
  fzf="fzf --cycle --reverse" \
  diff="diff --color=auto" \
  grep="grep --color=auto" \
  calc="bc -l" \
  cp="cp -ivr" \
  mv="mv -ivf" \
  dfh="df -h | grep -v '\s/dev.*$\|\s/run.*$\|\s/boot.*$'" \
  qrpng="qrencode -s 16 -o qr.png" \
  qr="qrencode -t ansiutf8" \
  wget='wget --hsts-file="${XDG_DATA_HOME:-$HOME/.local/share}"/wget/wget-hsts' \
  lg="lazygit" \
  adb='HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/android adb'

# See: https://yazi-rs.github.io/docs/quick-start#shell-wrapper
if command -v yazi >/dev/null 2>&1; then
  y() {
    set -- "$@" --cwd-file "$(mktemp -t yazi-cwd.XXXXXX)"
    command yazi "$@"
    shift $(($# - 1))
    set -- "$(
      command cat <"$1"
      printf .
      rm -f -- "$1"
    )"
    set -- "${1%.}"
    [ "$1" != "$PWD" ] && [ -d "$1" ] && command cd -- "$1"
  }
fi

if command -v zoxide >/dev/null 2>&1; then
  eval "$(zoxide init posix --hook prompt)"
fi
