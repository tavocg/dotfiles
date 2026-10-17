# Tavo's dotfiles

## Usage

```sh
cp -r ~/.config ~/.config.bak
git clone https://github.com/tavocg/dotfiles ~/.config

cat >> ~/.bashrc <<'SH'

[ -f ~/.config/bashrc ] && . ~/.config/bashrc\n
SH

cat >> ~/.profile <<'SH'

if [ -d ~/.config/profile.d ]; then
  for p in ~/.config/profile.d/*.sh; do
    if [ -f "$p" ] && [ -r "$p" ]; then
      . "$p"
    fi
  done
fi
SH
```
