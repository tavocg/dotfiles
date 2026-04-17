# shellcheck shell=sh

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
  cal="calcurse" \
  fzf="fzf --cycle --reverse" \
  diff="diff --color=auto" \
  grep="grep --color=auto" \
  calc="bc -l" \
  cp="cp -iv" \
  mv="mv -iv" \
  dfs="df -h | grep -v '\s/dev.*$\|\s/run.*$\|\s/boot.*$'" \
  qr-png="qrencode -s 16 -o qr.png" \
  qr="qrencode -t ansiutf8" \
  wget='wget --hsts-file="${XDG_DATA_HOME:-$HOME/.local/share}"/wget/wget-hsts' \
  lg="lazygit" \
  adb='HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/android adb'

pandoc-md2pdf() {
  file="$1"
  pandoc -f markdown+tex_math_single_backslash --pdf-engine=tectonic "$file" -o "${file%.md}.pdf"
}
