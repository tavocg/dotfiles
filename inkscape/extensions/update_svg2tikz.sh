#!/bin/sh

RAW_GITHUB_URL="https://raw.githubusercontent.com/xyz2tex/svg2tikz/refs/heads/master/svg2tikz"
INKSCAPE_EXTENSIONS="$HOME/.config/inkscape/extensions"
FILES="tikz_export.py tikz_export_effect.inx tikz_export_output.inx"

for file in $FILES; do
    wget -qO "$INKSCAPE_EXTENSIONS/$file" "$RAW_GITHUB_URL/$file"
done
