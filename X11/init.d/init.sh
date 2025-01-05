STATUS_BLOCKS="tray vol mic net bat kbd time date" dwmstatus &
dbus-update-activation-environment --systemd --all
systemctl --user import-environment DISPLAY
xset r rate 300 70
setxkbmap en
#clipmenud &
#dunst
#picom --experimental-backends --animation-for-open-window=zoom &
#emacs --daemon &
#xrandr --output DVI-D-0 --right-of HDMI-0
xsetroot -solid '#1e2326'

clear
sleep 0.2
