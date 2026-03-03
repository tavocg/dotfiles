#!/bin/sh

list="./list.sh"

. "$list"

set -ex
npm install -g $PACKAGES
