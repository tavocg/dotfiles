#!/bin/sh

. "/etc/os-release"
if [ "$ID" != "debian" ]; then
  exit 0
fi

list="./list.sh"

. "$list"

set -ex
sudo apt-get install -y $PACKAGES
