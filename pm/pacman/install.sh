#!/bin/sh

list="./list.sh"

. "$list"

sudo pacman -S $PACKAGES
