#!/bin/sh
# Install Nvidia proprietary driver

# Debian
sudo apt install -y linux-headers-amd64
sudo apt install -y nvidia-kernel-dkms nvidia-driver firmware-misc-nonfree
