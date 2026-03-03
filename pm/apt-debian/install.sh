#!/bin/sh

list="./list.sh"

. "$list"

set -ex
sudo apt-get install -y $PACKAGES
