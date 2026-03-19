if has('termguicolors') && ($COLORTERM ==# 'truecolor' || $COLORTERM ==# '24bit')
  set termguicolors
endif

let g:everforest_background = 'hard'
set background=dark

try
  colorscheme everforest
catch
endtry
