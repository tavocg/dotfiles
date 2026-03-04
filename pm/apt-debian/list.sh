# System
PACKAGES="
xdg-desktop-portal
xdg-desktop-portal-gtk
libglib2.0-bin
lxpolkit
pkexec
udisks2
gnome-disk-utility
perl
bluez
upower
libavcodec-extra
libfuse2
brightnessctl
"

# Network
PACKAGES="$PACKAGES
resolvconf
"

# Shared
PACKAGES="$PACKAGES
fonts-jetbrains-mono
ttf-mscorefonts-installer
"

# Window Manager - X11
PACKAGES="$PACKAGES
xorg
i3-wm
alacritty
suckless-tools
scrot
xdotool
arandr
"

# Window Manager - Wayland
PACKAGES="$PACKAGES
xdg-desktop-portal-wlr
xwayland
sway
foot
grim
slurp
"

# Audio
PACKAGES="$PACKAGES
pipewire
pipewire-alsa
pipewire-pulse
pulseaudio-utils
pipewire-jack
wireplumber
libspa-0.2-libcamera
libspa-0.2-bluetooth
alsa-tools
alsa-utils
"

# Media
PACKAGES="$PACKAGES
mpv
vlc
zathura
zathura-cb
zathura-djvu
zathura-pdf-poppler
zathura-ps
poppler-utils
obs-studio
"

# Tools
PACKAGES="$PACKAGES
bemenu
libbemenu-curses
libbemenu-wayland
libbemenu-x11
curl
fzf
eza
fd-find
ripgrep
pass
pass-otp
zbar-tools
trash-cli
unzip
imagemagick
tmux
bat
nnn
exiftool
groff
grap
zoxide
tokei
git
lazygit
shellcheck
jq
android-sdk-platform-tools
btop
python3-venv
7zip
imv
r-base
r-base-dev
rsync
ledger
resvg
"

# Appearance
PACKAGES="$PACKAGES
gnome-themes-extra
"

# Dev
PACKAGES="$PACKAGES
build-essential
python-is-python3
python3-pip
luarocks
lua5.4
libssl-dev
rustup
ruby-full
composer
php-cli
python3-pynvim
python3-numpy
libyaml-dev
libffi-dev
default-jdk
default-jre
r-cran-data.table
r-cran-openxlsx
r-cran-knitr
r-cran-rmarkdown
r-cran-tinytex
"

# Extra
PACKAGES="$PACKAGES
flatpak
steam-devices
"
