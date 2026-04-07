# shellcheck shell=sh

if [ -e "${XDG_CONFIG_HOME:-$HOME/.config}"/ssh/config ]; then
  export SSH_CONFIG="-F ${XDG_CONFIG_HOME:-$HOME/.config}/ssh/config"
  export GIT_SSH_COMMAND="ssh ${SSH_CONFIG}"
  alias ssh="ssh ${SSH_CONFIG}"
  alias scp="scp ${SSH_CONFIG}"
  alias rsync="rsync --rsh \"ssh ${SSH_CONFIG}\""
fi
