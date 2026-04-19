if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit')
  set termguicolors
endif

try
  set background=dark
  colorscheme everforest
catch
endtry
