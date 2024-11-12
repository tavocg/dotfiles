#!/bin/sh

podman run -d --restart=always --name dev -p 2222:22 --replace arch-dev-env
