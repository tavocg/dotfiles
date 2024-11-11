#!/bin/sh

podman rm -f $(podman ps -a -q)
podman rmi -f $(podman images -a -q)
podman volume rm $(podman volume ls -q)
podman network prune -f
podman system prune -a -f
