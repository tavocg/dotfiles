#!/bin/sh

if ! command -v sudo >/dev/null 2>&1; then
  alias sudo="doas"
  complete -cf doas
fi

alias \
  src="cd $HOME/.local/src/ && ls" \
  cfg="cd $HOME/.config/ && ls" \
  tmp="cd $HOME/Desktop/temp/ && ls" \
  dsk="cd $HOME/Desktop/ && ls" \
  prj="cd $HOME/Projects && ls" \
  doc="cd $HOME/Documents/ && ls" \
  dow="cd $HOME/Downloads/ && ls" \
  mus="cd $HOME/Music/ && ls" \
  prt="cd $HOME/Pictures/Screenshots/ && ls" \
  bkg="cd $HOME/Pictures/Backgrounds/ && ls" \
  img="cd $HOME/Pictures/ && ls" \
  vid="cd $HOME/Videos/ && ls"

EZA_OPTS="--git --group-directories-first --icons --time-style=long-iso"
command -v exa >/dev/null 2>&1 &&
  alias la="exa $EZA_OPTS -alghUum" &&
  alias lt="exa $EZA_OPTS -T -L 2" &&
  alias ll="exa $EZA_OPTS -alg" &&
  alias ls="exa $EZA_OPTS -1"

command -v eza >/dev/null 2>&1 &&
  alias la="eza $EZA_OPTS -alghUum" &&
  alias lt="eza $EZA_OPTS -T -L 2" &&
  alias ll="eza $EZA_OPTS -alg" &&
  alias ls="eza $EZA_OPTS -1"

command -v trash >/dev/null 2>&1 && alias rm="trash"

alias \
  cal="calcurse" \
  fzf="fzf --cycle --reverse" \
  diff="diff --color=auto" \
  grep="grep --color=auto" \
  calc="bc -l" \
  cp="cp -iv" \
  mv="mv -iv" \
  df-short="df -h | grep -v '\s/dev.*$\|\s/run.*$\|\s/boot.*$'" \
  qr-png="qrencode -s 16 -o qr.png" \
  qr="qrencode -t ansiutf8" \
  clip="xsel -ib" \
  wget="wget --hsts-file=$XDG_DATA_HOME/wget/wget-hsts" \
  ssh="ssh ${SSH_CONFIG}" \
  scp="scp ${SSH_CONFIG}" \
  rsync="rsync --rsh \"ssh ${SSH_CONFIG}\"" \
  lg="lazygit" \
  lvim="VIMINIT= nvim"
