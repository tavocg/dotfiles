#!/bin/sh

echo "HOSTNAME=''" >> ~/.bash_profile
echo ". ~/.config/shell/env-min" >> ~/.bash_profile
echo ". ~/.config/shell/bashrc" >> ~/.bashrc

sudo pacman -S --needed --noconfirm sudo man-db exa curl jq ffmpeg imagemagick \
    nnn fd screen tmux groff openssh base-devel git neovim ripgrep fzf emacs \
    python python-pip python-virtualenv python-pipx python-pynvim go go-tools \
    clang nodejs npm php cargo r texlive-basic texlive-latexrecommended \
    texlive-plaingeneric texlive-fontsextra shellcheck bash-language-server \
    shfmt zshdb gopls tree-sitter-cli cmake tidy stylelint pandoc composer

git clone git@git.tavo.one:tavo/dotfiles.git ~/.config

sudo pacman -S --needed git base-devel
sudo mkdir -p /opt/yay
sudo chown -R dev:dev /opt/yay
git clone https://aur.archlinux.org/yay.git /opt/yay
(cd /opt/yay && makepkg -si --noconfirm)

LV_BRANCH='release-1.4/neovim-0.9' bash <(curl -s https://raw.githubusercontent.com/LunarVim/LunarVim/release-1.4/neovim-0.9/utils/installer/install.sh)
