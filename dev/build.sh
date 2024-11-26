#!/bin/sh

FJP_ARG="$1"
[ -z "$FJP_ARG" ] && FJP_ARG="$(pass personal/forgejo | sed '/public-keys/!d;s/^.*: //')"
[ -z "$FJP_ARG" ] && echo "No forgejo token provided" && exit 1

podman rm -f dev
podman build --build-arg FJP="$FJP_ARG" -t arch-dev-env .
