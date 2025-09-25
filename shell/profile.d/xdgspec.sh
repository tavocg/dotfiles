#!/bin/sh

# For some reason, this breaks xdg-desktop-portal
# XDG_DATA_DIRS="$HOME/.local/share/flatpak/exports/share:/var/lib/flatpak/exports/share:/usr/local/share/applications:/usr/share/applications:$XDG_DATA_DIRS" \

export \
  XDG_STATE_HOME="$HOME/.local/state" \
  XDG_CACHE_HOME="$HOME/.local/cache" \
  XDG_DATA_HOME="$HOME/.local/share" \
  XDG_BIN_HOME="$HOME/.local/bin" \
  XDG_CONFIG_HOME="$HOME/.config"

export \
  XDG_DOCUMENTS_DIR="$HOME/Documents" \
  XDG_DOWNLOAD_DIR="$HOME/Downloads" \
  XDG_PICTURES_DIR="$HOME/Pictures" \
  XDG_DESKTOP_DIR="$HOME/Desktop" \
  XDG_VIDEOS_DIR="$HOME/Videos" \
  XDG_MUSIC_DIR="$HOME/Music"

export \
  _JAVA_OPTIONS=-Djava.util.prefs.userRoot="$XDG_CONFIG_HOME/java" \
  MATHEMATICA_USERBASE="$XDG_CONFIG_HOME/mathematica" \
  XCURSOR_PATH=/usr/share/icons:"$XDG_DATA_HOME"/icons \
  GTK2_RC_FILES="$XDG_CONFIG_HOME/gtk-2.0/gtkrc-2.0" \
  PASSWORD_STORE_DIR="$XDG_DATA_HOME/password-store" \
  TEXMFCONFIG="$XDG_CONFIG_HOME/texlive/texmf-config" \
  TEXMFVAR="$XDG_CACHE_HOME/texlive/texmf-var" \
  TEXMFHOME="$XDG_DATA_HOME/texmf" \
  VIMINIT="source $XDG_CONFIG_HOME/vim/vimrc" \
  XSERVERRC="$XDG_CONFIG_HOME/X11/xserverrc" \
  MBSYNCRC="$XDG_CONFIG_HOME/isync/mbsyncrc" \
  XAUTHORITY="$XDG_RUNTIME_DIR/Xauthority" \
  XINITRC="$XDG_CONFIG_HOME/X11/xinit/xinitrc" \
  LESSHISTFILE="$XDG_DATA_HOME/lesshst" \
  ELECTRUMDIR="$XDG_DATA_HOME/electrum" \
  CUDA_CACHE_PATH="$XDG_CACHE_HOME/nv" \
  WINEPREFIX="$XDG_DATA_HOME/wine" \
  WGETRC="$XDG_CONFIG_HOME/wget/wgetrc" \
  GNUPGHOME="$XDG_DATA_HOME/gnupg" \
  GOPATH="$XDG_DATA_HOME/go" \
  SSH_CONFIG="-F ${XDG_CONFIG_HOME}/ssh/config" \
  GIT_SSH_COMMAND="ssh -F ${XDG_CONFIG_HOME}/ssh/config" \
  ZDOTDIR="$XDG_CONFIG_HOME/shell" \
  NPM_CONFIG_USERCONFIG="$XDG_CONFIG_HOME/npm/npmrc" \
  IPYTHONDIR="$XDG_CONFIG_HOME/ipython" \
  PYENV_ROOT="$XDG_DATA_HOME/pyenv" \
  PYTHON_HISTORY="$XDG_CACHE_HOME/python_history" \
  SQLITE_HISTORY="$XDG_CACHE_HOME/sqlite_history" \
  PGPASSFILE="$XDG_CONFIG_HOME/pg/pgpass" \
  CARGO_HOME="$XDG_DATA_HOME"/cargo \
  RUSTUP_HOME="$XDG_DATA_HOME"/rustup \
  UNISON="$XDG_DATA_HOME"/unison
