# shellcheck shell=sh

export ANDROID_USER_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/android
export ANDROID_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/android/sdk

if ! [ -d "$ANDROID_USER_HOME" ]; then
  mkdir -p "$ANDROID_USER_HOME"
fi

if ! [ -d "$ANDROID_HOME" ]; then
  mkdir -p "$ANDROID_HOME"
fi

export PATH="$ANDROID_HOME/emulator${PATH:+:${PATH}}"
export PATH="$ANDROID_HOME/platform-tools${PATH:+:${PATH}}"
