if [ -n "$XDG_DATA_HOME" ] ; then
    export ANDROID_USER_HOME="$XDG_DATA_HOME"/android
    export ANDROID_HOME=$XDG_DATA_HOME/Android/Sdk
else
    export ANDROID_USER_HOME="$HOME"/.local/share/android
    export ANDROID_HOME=$HOME/.local/share/Android/Sdk
fi

if ! [ -f "$ANDROID_USER_HOME" ] ; then
    mkdir -p "$ANDROID_USER_HOME"
fi

if ! [ -f "$ANDROID_HOME" ] ; then
    mkdir -p "$ANDROID_HOME"
fi

alias adb='HOME="$ANDROID_USER_HOME" adb'

export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools
