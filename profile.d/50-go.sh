# shellcheck shell=sh

godir="${XDG_DATA_HOME:-$HOME/.local/share}/go"

case ":$PATH:" in
*":$godir/bin:"*) ;;
*) export PATH="$godir/bin${PATH:+:${PATH}}" ;;
esac

export GOTOOLCHAIN=auto

mkdir -p "$godir"/sdk

for sdk in "$godir"/sdk/go*; do
  if [ -d "$sdk" ]; then
    case ":$PATH:" in
    *":$sdk/bin:"*) ;;
    *) export PATH="$sdk/bin${PATH:+:${PATH}}" ;;
    esac
  fi
done
