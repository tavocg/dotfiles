# shellcheck shell=sh

export ANDROID_USER_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/android
export ANDROID_HOME="${XDG_DATA_HOME:-$HOME/.local/share}"/android/sdk

if ! [ -d "$ANDROID_USER_HOME" ]; then
  mkdir -p "$ANDROID_USER_HOME"
fi

if ! [ -d "$ANDROID_HOME" ]; then
  mkdir -p "$ANDROID_HOME"
fi

case ":$PATH:" in
*":$ANDROID_HOME/emulator:"*) ;;
*) export PATH="$ANDROID_HOME/emulator${PATH:+:${PATH}}" ;;
esac

case ":$PATH:" in
*":$ANDROID_HOME/platform-tools:"*) ;;
*) export PATH="$ANDROID_HOME/platform-tools${PATH:+:${PATH}}" ;;
esac
