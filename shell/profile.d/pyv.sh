#!/usr/bin/env bash

for f in pyv pyv_comp.bash ; do
    if [ -f ~/.local/share/pyv/$f ] ; then
        . ~/.local/share/pyv/$f
    fi
done
