#!/bin/sh

if ! command -v rustup >/dev/null 2>&1; then
  exit 1
fi

if ! command -v cargo >/dev/null 2>&1; then
  exit 1
fi
