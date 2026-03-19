set number
set cursorline
set smartcase
set wildmode=list:longest
set list
set listchars=tab:▏\ ,trail:~

if has('clipboard')
  set clipboard=unnamedplus
endif

nnoremap Y y$

nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-h> <c-w>h
nnoremap <c-l> <c-w>l

noremap <c-up> <c-w>+
noremap <c-down> <c-w>-
noremap <c-left> <c-w>>
noremap <c-right> <c-w><

inoremap hh <Esc>hh
inoremap jj <Esc>jj
inoremap kk <Esc>kk
inoremap ll <Esc>ll
inoremap gg <Esc>gg

" Restore cursor position
autocmd BufReadPost * normal! g`"

" Restore folds
augroup SaveView
  autocmd!
  autocmd BufWinLeave * if &buftype == '' && expand('%') != '' | mkview | endif
  autocmd BufWinEnter * if &buftype == '' && expand('%') != '' | silent! loadview | endif
augroup END
