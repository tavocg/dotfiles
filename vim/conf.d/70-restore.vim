set undofile

" Restore cursor position
augroup RestoreCursor
  autocmd!
  autocmd BufReadPost *
        \ if line("'\"") >= 1 && line("'\"") <= line('$') |
        \   execute 'normal! g`"' |
        \ endif
augroup END

" Restore folds
augroup SaveView
  autocmd!
  autocmd BufWinLeave *
        \ if &buftype == '' && expand('%') != '' | mkview | endif
  autocmd BufWinEnter *
        \ if &buftype == '' && expand('%') != '' | silent! loadview | endif
augroup END
