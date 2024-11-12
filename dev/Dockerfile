FROM docker.io/library/archlinux:latest

RUN pacman -Syu --noconfirm && \
    pacman -S --noconfirm sudo exa curl ffmpeg imagemagick groff openssh base-devel git neovim ripgrep emacs python python-pip python-virtualenv go go-tools clang nodejs npm php texlive-core texlive-fontsextra r && \
    mkdir /var/run/sshd

RUN useradd -ms /bin/bash dev && \
    echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

ARG SSH_KEY
RUN mkdir -p /home/dev/.ssh && \
    ssh-keygen -A && \
    echo "$SSH_KEY" >> /home/dev/.ssh/authorized_keys && \
    chown -R dev:dev /home/dev/.ssh && \
    chmod 600 /home/dev/.ssh/authorized_keys

RUN git clone https://git.tavo.one/tavo/dotfiles /home/dev/.config && \
    echo "HOSTNAME='dev'" >> /home/dev/.bash_profile && \
    echo ". ~/.config/shell/env-min" >> /home/dev/.bash_profile && \
    echo ". ~/.config/shell/bashrc" >> /home/dev/.bashrc && \
    chown -R dev:dev /home/dev

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
