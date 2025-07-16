if [ -f "$XDG_DATA_HOME" ] ; then
    export ANDROID_HOME=$XDG_DATA_HOME/Android/Sdk
else
    export ANDROID_HOME=$HOME/Android/Sdk
fi

export PATH=$PATH:$ANDROID_HOME/emulator
export PATH=$PATH:$ANDROID_HOME/platform-tools
