#!/bin/sh

if ! command -v rustup >/dev/null 2>&1; then
  exit 0
fi

if ! command -v cargo >/dev/null 2>&1; then
  exit 0
fi

tc="$(rustup show active-toolchain)"
if [ -z "$tc" ]; then
  rustup default stable
fi

set -ex

if ! command -v cargo-install-update >/dev/null 2>&1; then
  cargo install cargo-update
fi

cargo install-update -a
