#!/bin/sh

. "/etc/os-release"
if [ "$ID" != "debian" ]; then
  exit 1
fi
