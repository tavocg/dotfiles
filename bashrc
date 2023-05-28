#!/bin/bash
# tavo custom bashrc file

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# Prompt
PS1='\[\e[0;1;91m\][\[\e[0;1;38;5;87m\]\u\[\e[0;0;38;5;123m\]@\[\e[0;0;38;5;159m\]\h \[\e[0;38;5;247m\]\w\[\e[0;1;91m\]]\n \[\e[0;38;5;247m\]$ \[\e[0m\]'
PROMPT_COMMAND="echo"                           # Line jump after every command

# General configs
PATH="$HOME/.config/scripts${PATH:+:${PATH}}"   # Enables custom scripts dir
bind "set completion-ignore-case on"            # Case insensitive TAB complete
shopt -s cdspell                                # Autocorrect directory name
shopt -s autocd                                 # cd into dir by typing the name
set -o vi                                       # vi mode

# $HOME cleanup
export \
    LESSHISTFILE="$HOME/.local/share/lesshst" \
    VIMINIT="source $HOME/.config/vim/vimrc" \
    XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" \
    HISTFILE="$XDG_STATE_HOME/bash_history" \
    CUDA_CACHE_PATH="$XDG_CACHE_HOME"/nv \
    XDG_STATE_HOME="$HOME/.local/state" \
    XDG_CACHE_HOME="$HOME/.local/cache" \
    XDG_DATA_HOME="$HOME/.local/share" \
    XDG_CONFIG_HOME="$HOME/.config" \

# Aliases
alias \
    prt="cd $HOME/Pictures/Screenshots/ ; ls" \
    bkg="cd $HOME/Pictures/Backgrounds/ ; ls" \
    tmp="cd $HOME/Desktop/temp/ ; ls" \
    www="cd $HOME/Desktop/www/ ; ls" \
    src="cd $HOME/.local/src/ ; ls" \
    doc="cd $HOME/Documents/ ; ls" \
    dow="cd $HOME/Downloads/ ; ls" \
    img="cd $HOME/Pictures/ ; ls" \
    dsk="cd $HOME/Desktop/ ; ls" \
    cfg="cd $HOME/.config/ ; ls" \
    vid="cd $HOME/Videos/ ; ls" \
    mus="cd $HOME/Music/ ; ls" \
    ent="cd /mnt/Entr/ ; ls" \
    ls="exa -al --group-directories-first" \
    diff="diff --color=auto" \
    grep="grep --color=auto" \
    def="dict" \
    calc="bc -l" \
    cp="cp -iv" \
    mv="mv -iv" \
    rm="rm -iv" \
    vim="nvim" \

# Useful variables
export \
    GITLAB="ssh://git@gitlab.com/tavo-wasd" \
    QT_QPA_PLATFORMTHEME="qt6ct" \
    DATE=$(date -I) \

# Clipboard config
export \
    CM_SELECTIONS="clipboard" \
    CM_MAX_CLIPS=10 \

# Default programs
export \
    OPENER="xdg-open" \
    READER="zathura" \
    BROWSER="brave" \
    TERMINAL="st" \
    EDITOR="nvim" \
    VISUAL="nvim" \
    IMAGE="sxiv" \
    VIDEO="mpv" \

# Functions
follow() { # cd if found after 'which' command
[ -z "$1" ] && echo "Usage: follow <command>" ||
    cd "$(which $1 | sed s/$1//g)"
}

follow-edit() { # open in $EDITOR
[ -z "$1" ] && echo "Usage: follow <command>" ||
    $EDITOR "$(which $1)"
}

quitcd() { # cd into NNN's exported dir: Ctrl + G
cd $(grep -oP '"\K[^"\047]+(?=["\047])' ~/.config/nnn/.lastd)
}
bind '"\C-g":"quitcd\C-m"'

# Autostart dwm after tty login
type systemctl 2>/dev/null 1>&2 && if systemctl -q is-active graphical.target && [[ ! $DISPLAY && $XDG_VTNR -eq 1 ]]; then
    cat ~/.config/snippets/welcome
    sleep 0.5
    amixer
    exec startx
fi

[ "/usr/local/bin/afetch" ] && afetch || fetch min
