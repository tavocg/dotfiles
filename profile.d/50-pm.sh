# shellcheck shell=sh

if [ -n "$BASH_VERSION" ] && command -v pm >/dev/null 2>&1; then
  eval "$(pm completion bash)"
fi
