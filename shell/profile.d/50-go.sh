# shellcheck shell=sh

godir="${XDG_DATA_HOME:-$HOME/.local/share}"/go

export PATH="$godir/bin${PATH:+:${PATH}}"

export GOTOOLCHAIN=auto

mkdir -p "$godir"/sdk

for sdk in "$godir"/sdk/go*; do
  if [ -d "$sdk" ]; then
    export PATH="$sdk/bin:${PATH:+:${PATH}}"
  fi
done
