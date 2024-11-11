#!/bin/sh

SSH_KEY_ARG="$1"
[ -z "$SSH_KEY_ARG" ] && echo "No ssh-key provided" && exit 1

podman rm -f dev
podman build --build-arg SSH_KEY="$SSH_KEY_ARG" -t debian-dev-env .
