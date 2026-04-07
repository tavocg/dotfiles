# shellcheck shell=sh

for d in mesa_shader_cache mesa_shader_cache_db; do
  if [ -d "${XDG_CACHE_HOME:-$HOME/.local/cache}" ] && ! [ -d "${XDG_CACHE_HOME:-$HOME/.local/cache}/$d" ]; then
    mkdir -p "${XDG_CACHE_HOME:-$HOME/.local/cache}/$d"
  fi
done
