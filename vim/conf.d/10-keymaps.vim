function! Sanitize()
  let l:view = winsaveview()
  %s/\s\+$//e
  %s/\n\+\%$//e
  normal! gg=G
  call winrestview(l:view)
endfunction

nnoremap <C-s> :call Sanitize()<Bar>update<CR>
inoremap <C-s> <Esc>:call Sanitize()<Bar>update<CR>a
vnoremap <C-s> <Esc>:call Sanitize()<Bar>update<CR>gv

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
