FROM debian:latest

RUN apt-get update && \
    apt-get install -y sudo exa curl ffmpeg imagemagick groff openssh-server build-essential git neovim emacs python3 python-is-python3 python3-venv python3-pip golang clang clang-format nodejs npm php-fpm r-recommended texlive-base texlive-fonts-recommended && \
    mkdir /var/run/sshd

RUN useradd -ms /bin/bash dev
RUN echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

ARG SSH_KEY

RUN mkdir -p /home/dev/.ssh && \
    echo "$SSH_KEY" >> /home/dev/.ssh/authorized_keys && \
    chown -R dev:dev /home/dev/.ssh && \
    chmod 600 /home/dev/.ssh/authorized_keys

RUN git clone https://git.tavo.one/tavo/dotfiles /home/dev/.config && \
    echo ". ~/.config/shell/env-min" >> /home/dev/.bash_profile && \
    echo ". ~/.config/shell/env-min" >> /home/dev/.bashrc && \
    echo ". ~/.config/shell/bashrc" >> /home/dev/.bash_profile && \
    echo ". ~/.config/shell/bashrc" >> /home/dev/.bashrc && \
    chown -R dev:dev /home/dev

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
