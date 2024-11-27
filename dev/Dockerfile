FROM docker.io/library/archlinux:latest

RUN pacman -Syu --noconfirm && \
    pacman -S --needed --noconfirm sudo curl jq openssh git && \
    mkdir -p /var/run/sshd && \
    ssh-keygen -A && \
    useradd -ms /bin/bash dev && \
    echo "dev ALL=(ALL) NOPASSWD:ALL" >> /etc/sudoers

USER dev

ARG FJP

RUN mkdir -p ~/.ssh && \
    ssh-keygen -t ed25519 -f ~/.ssh/id_ed25519 -N "" && \
    curl -sH "Authorization: token $FJP" https://git.tavo.one/api/v1/users/tavo/keys | \
        jq | grep '"key"' | cut -d '"' -f 4 >> ~/.ssh/authorized_keys && \
    chmod 600 ~/.ssh/authorized_keys && \
    NEW_KEY=$(cat ~/.ssh/id_ed25519.pub | cut -d' ' -f 1,2) && \
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
        -d "$JSON" && \
    ssh-keyscan -H git.tavo.one >> ~/.ssh/known_hosts && \
    chmod 600 ~/.ssh/known_hosts

USER root

EXPOSE 22
CMD ["/usr/sbin/sshd", "-D"]
