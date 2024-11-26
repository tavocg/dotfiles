FROM docker.io/library/archlinux:latest

RUN pacman -Syu --noconfirm && \
    pacman -S --needed --noconfirm sudo man-db exa curl jq ffmpeg imagemagick nnn \
        screen tmux groff openssh base-devel git neovim ripgrep fzf emacs \
        python python-pip python-virtualenv go go-tools clang nodejs npm php \
        cargo texlive-core texlive-fontsextra r && \
    mkdir -p /var/run/sshd

RUN ssh-keygen -A

RUN useradd -ms /bin/bash dev && \
    echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER dev

RUN echo "HOSTNAME=''" >> ~/.bash_profile && \
    echo ". ~/.config/shell/env-min" >> ~/.bash_profile && \
    echo ". ~/.config/shell/bashrc" >> ~/.bashrc

RUN mkdir -p ~/.ssh && \
    ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519 -N ""

ARG FJP

# Allow ssh from keys in foregejo instance
RUN curl -sH "Authorization: token $FJP" https://git.tavo.one/api/v1/users/tavo/keys | \
        jq | grep '"key"' | cut -d '"' -f 4 >> ~/.ssh/authorized_keys && \
    chmod 600 ~/.ssh/authorized_keys

# Add new "dev" key in the forgejo instance
RUN NEW_KEY=$(cat ~/.ssh/id_ed25519.pub | cut -d' ' -f 1,2) && \
    OLD_KEY="$(curl -s "https://git.tavo.one/api/v1/users/tavo/keys" \
        -H "Authorization: token $FJP" \
        -H "accept: application/json" \
        -H "Content-Type: application/json" | \
        jq -r '.[] | select(.title == "dev") | .id')" && \
    if [ -n "$OLD_KEY" ] ; then \
        curl -sX DELETE "https://git.tavo.one/api/v1/user/keys/$OLD_KEY" \
            -H "Authorization: token $FJP" \
            -H "accept: application/json" \
            -H "Content-Type: application/json"; \
    fi && \
    JSON=$(printf '{"key": "%s", "read_only": true, "title": "dev"}' "$NEW_KEY") && \
    curl -s "https://git.tavo.one/api/v1/user/keys" \
        -H "Authorization: token $FJP" \
        -H "accept: application/json" \
        -H "Content-Type: application/json" \
        -d "$JSON"

# Add SSH Host Key for git.tavo.one and clone
RUN ssh-keyscan -H git.tavo.one >> ~/.ssh/known_hosts && \
    chmod 600 ~/.ssh/known_hosts

RUN git clone git@git.tavo.one:tavo/dotfiles.git ~/.config

RUN sudo pacman -S --needed git base-devel && \
    sudo mkdir -p /opt/yay && \
    sudo chown -R dev:dev /opt/yay && \
    git clone https://aur.archlinux.org/yay.git /opt/yay && \
    (cd /opt/yay && makepkg -si --noconfirm)

RUN LV_BRANCH='release-1.4/neovim-0.9' \
    bash <(curl -s https://raw.githubusercontent.com/LunarVim/LunarVim/release-1.4/neovim-0.9/utils/installer/install.sh)

USER root

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
