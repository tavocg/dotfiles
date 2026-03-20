" Sanitize
"
" Runs sanitizer on BufWritePre
"
" Default behavior:
"   - Remove trailing whitespace
"   - Remove trailing blank lines
"   - Reindent entire file
"
" ftplugins can override by setting:
"   let b:sanitize_func = function('s:MySanitize')

function! s:DefaultSanitize()
  let l:view = winsaveview()
  silent! %s/\s\+$//e
  silent! %s/\n\+\%$//e
  silent! normal! gg=G
  call winrestview(l:view)
endfunction

function! Sanitize()
  if exists('b:sanitize_func')
    call call(b:sanitize_func, [])
  else
    call s:DefaultSanitize()
  endif
endfunction

augroup sanitize_on_save
  autocmd!
  autocmd BufWritePre * if &filetype !~# '^\(txt\|markdown\)$' | call Sanitize() | endif
augroup END
