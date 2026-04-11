# shellcheck shell=sh

if [ -n "$BASH_VERSION" ] && command -v pm >/dev/null 2>&1; then
  if pm_out="$(pm completion bash 2>/dev/null)"; then
    eval "$pm_out"
  fi
fi
