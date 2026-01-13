# Use XDG spec with ssh, ultimately gave up on this beacause i need SFTP clients to work,
# some allow to configure custom private key locations but other don't.

# Set to something like 1 to enable
USE_XDG_SSH_DIR=

if [ "$USE_XDG_SSH_DIR" ] && [ -d "$XDG_CONFIG_HOME" ]; then
  export SSH_CONFIG="-F ${XDG_CONFIG_HOME}/ssh/config"

  export GIT_SSH_COMMAND="ssh ${SSH_CONFIG}"
  alias ssh="ssh ${SSH_CONFIG}"
  alias scp="scp ${SSH_CONFIG}"
  alias rsync="rsync --rsh \"ssh ${SSH_CONFIG}\""
fi
