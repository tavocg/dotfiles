# sboxes

Keep in mind, the macro utils.tmac sources sboxes.tmac, but
that macro is exclusive to groff v1.23.0, which is not
present on Debian 12. Compiling it from source is really
easy though, thanks to GNU's coding standards.

# Custom fonts & glyphs

In order to install a font with U+2112 '_Laplace transform_'
glyph, search in your system for fonts with that glyph, and
create a groff font with afmtodit.

```shell
grep -r '2112' /usr/share/fonts/* 2>/dev/null
afmtodit -e text.enc -i0 -m path-to-font.afm /usr/share/groff/current/font/devpdf/map/textmap NAME
mkdir -p /usr/share/groff/site-font/devpdf/ # Create this directory if not already present
mv NAME /usr/share/groff/site-font/devpdf/
```
