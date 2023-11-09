# sboxes

Keep in mind, the macro utils.tmac sources sboxes.tmac, but
that macro is exclusive to groff v1.23.0, which is not
present on Debian 12. Compiling it from source is really
easy though, thanks to GNU's coding standards.

# Custom fonts & glyphs

In order to install the U-BMD font,
required for U+2112 '_Laplace transform_' glyph, just copy the
provided file to groff's site-font directory, like so:

```shell
cp ~/.config/groff/site-font/U-BMD /usr/share/groff/site-font/devpdf/U-BMD
```
