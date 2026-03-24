# shellcheck shell=sh

godir="$XDG_DATA_HOME"/go

export PATH="$godir/bin${PATH:+:${PATH}}"

export GOTOOLCHAIN=auto
mkdir -p "$godir"/sdk

for sdk in "$godir"/sdk/go*; do
  export PATH="$sdk/bin:${PATH:+:${PATH}}"
done
