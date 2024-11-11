#!/bin/sh

podman rm -f dev
podman build --build-arg SSH_KEY="$(cat ~/.ssh/id_ed25519.pub)" -t debian-dev-env .
