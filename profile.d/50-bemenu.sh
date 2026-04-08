# shellcheck shell=sh

col_nb="#1e2326"
col_nf="#374145"
col_sb="#7fbbb3"
col_sf="#1e2326"

font="JetBrainsMono Medium 12"

BEMENU_OPTS="--no-exec -i -c -l 10 -W 0.4 -B 2 -H 30 --cw 1"
BEMENU_OPTS="$BEMENU_OPTS --tb '$col_sb' --tf '$col_sf'"
BEMENU_OPTS="$BEMENU_OPTS --fb '$col_nb' --ff '$col_nf'"
BEMENU_OPTS="$BEMENU_OPTS --cb '$col_nb' --cf '$col_sb'"
BEMENU_OPTS="$BEMENU_OPTS --nb '$col_nb' --nf '$col_nf'"
BEMENU_OPTS="$BEMENU_OPTS --hb '$col_sb' --hf '$col_sf'"
BEMENU_OPTS="$BEMENU_OPTS --ab '$col_nb' --af '$col_nf'"
BEMENU_OPTS="$BEMENU_OPTS --bdr '$col_sb'"
BEMENU_OPTS="$BEMENU_OPTS --fn '$font'"

alias bemenu='BEMENU_OPTS="$BEMENU_OPTS" bemenu'
alias bemenu-run='BEMENU_OPTS="$BEMENU_OPTS" bemenu-run'
