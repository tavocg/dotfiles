if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit')
  set termguicolors
endif

try
  let g:everforest_background = 'hard'
  set background=dark
  colorscheme everforest
catch
endtry
