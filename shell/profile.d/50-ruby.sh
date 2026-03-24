# shellcheck shell=sh

GEM_BASE="${XDG_DATA_HOME:-$HOME/.local/share}/gem/ruby"
latest_bin=""
latest_home=""

for d in "$GEM_BASE"/*/bin; do
  [ -d "$d" ] || continue
  latest_bin="$d"
  latest_home=${d%/bin}
done

if [ -n "$latest_bin" ]; then
  export GEM_HOME="$latest_home"
  case ":$PATH:" in
  *":$latest_bin:"*) ;;
  *) export PATH="$latest_bin${PATH:+:${PATH}}" ;;
  esac
fi
