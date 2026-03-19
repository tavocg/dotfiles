" Restore cursor position
autocmd BufReadPost * normal! g`"

" Restore folds
augroup SaveView
  autocmd!
  autocmd BufWinLeave * if &buftype == '' && expand('%') != '' | mkview | endif
  autocmd BufWinEnter * if &buftype == '' && expand('%') != '' | silent! loadview | endif
augroup END
