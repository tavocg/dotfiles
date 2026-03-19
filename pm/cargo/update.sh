#!/bin/sh

tc="$(rustup show active-toolchain)"
if [ -z "$tc" ]; then
  rustup default stable
fi

if ! command -v cargo-install-update >/dev/null 2>&1; then
  cargo install cargo-update
fi

cargo install-update -a
