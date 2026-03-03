#!/bin/sh

if ! command -v gem >/dev/null 2>&1; then
  return 0
fi

if ! command -v ruby >/dev/null 2>&1; then
  return 0
fi

set -ex
gem update --user-install
gem cleanup --user-install
