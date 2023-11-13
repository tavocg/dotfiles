# sboxes

Keep in mind, the macro utils.tmac sources sboxes.tmac, but
that macro is exclusive to groff v1.23.0, which is not
present on Debian 12. Compiling it from source is really
easy though, thanks to GNU's coding standards.

# To install CM font
```shell
wget www.ams.org/arc/tex/amsfonts.zip
unzip -d amsfonts amsfonts.zip
groff-install-font -F CM -f +R amsfonts/fonts/type1/public/amsfonts/cm/cmr10.pfb
groff-install-font -F CM -f +I amsfonts/fonts/type1/public/amsfonts/cm/cmmi10.pfb
groff-install-font -F CM -f +B amsfonts/fonts/type1/public/amsfonts/cm/cmbx10.pfb
groff-install-font -F CM -f +SS amsfonts/fonts/type1/public/amsfonts/euler/eusm10.pfb   \" Euler symbols
groff-install-font -F CM -f +SE amsfonts/fonts/type1/public/amsfonts/symbols/msam10.pfb \" Euler extra
groff-install-font -F CM -f +SB amsfonts/fonts/type1/public/amsfonts/symbols/msbm10.pfb \" Blackboard
groff-install-font -F CM -f +SF amsfonts/fonts/type1/public/amsfonts/euler/eufm10.pfb   \" Blackletter (Fraktur)
```

# Custom fonts & glyphs

In order to install a font with custom glyphs like
U+2112 '_Laplace transform_', search in your system for
fonts with that glyph, and create a groff font with afmtodit.

```shell
grep -r '2112' /usr/share/fonts/* 2>/dev/null # Check for .afm font files with the glyph
afmtodit -e text.enc -i0 -m PATH-TO-FONT.afm /usr/share/groff/current/font/devpdf/map/textmap NAME
mkdir -p /usr/share/groff/site-font/devpdf/ # Create this directory if not already present
mv NAME /usr/share/groff/site-font/devpdf/
```
