#!/bin/sh

. "/etc/os-release"
if [ "$ID" != "debian" ]; then
  exit 0
fi

set -ex
sudo apt-get update -y
