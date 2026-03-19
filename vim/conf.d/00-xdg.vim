let s:is_nvim = has('nvim')
let s:appname = s:is_nvim ? 'nvim' : 'vim'

let s:config_home = empty($XDG_CONFIG_HOME) ? expand('~/.config')      : $XDG_CONFIG_HOME
let s:data_home   = empty($XDG_DATA_HOME)   ? expand('~/.local/share') : $XDG_DATA_HOME
let s:state_home  = empty($XDG_STATE_HOME)  ? expand('~/.local/state') : $XDG_STATE_HOME
let s:cache_home  = empty($XDG_CACHE_HOME)  ? expand('~/.cache')       : $XDG_CACHE_HOME

let s:config = s:config_home . '/' . s:appname
let s:data   = s:data_home   . '/' . s:appname
let s:state  = s:state_home  . '/' . s:appname
let s:cache  = s:cache_home  . '/' . s:appname

for dir in [
      \ s:config . '/after',
      \ s:data   . '/undo',
      \ s:data   . '/spell',
      \ s:state  . '/view',
      \ s:cache  . '/swap',
      \ s:cache  . '/backup'
      \ ]
  call mkdir(dir, 'p', 0700)
endfor

if isdirectory(s:config)
  let s:rtp = split(&runtimepath, ',')

  if index(s:rtp, s:config) < 0
    call insert(s:rtp, s:config, 0)
  endif

  if isdirectory(s:config . '/after') && index(s:rtp, s:config . '/after') < 0
    call add(s:rtp, s:config . '/after')
  endif

  let &runtimepath = join(s:rtp, ',')
endif

if isdirectory(s:cache . '/swap')
  execute 'set directory=' . fnameescape(s:cache . '/swap') . '//,.,/tmp'
endif

if isdirectory(s:cache . '/backup')
  execute 'set backupdir=' . fnameescape(s:cache . '/backup') . '//,.,/tmp'
endif

if isdirectory(s:data . '/undo')
  execute 'set undodir=' . fnameescape(s:data . '/undo') . '//'
  set undofile
endif

if isdirectory(s:data . '/spell')
  execute 'set spellfile=' . fnameescape(s:data . '/spell/en.utf-8.add')
endif

if isdirectory(s:state . '/view')
  execute 'set viewdir=' . fnameescape(s:state . '/view') . '/'
endif

if s:is_nvim
  if isdirectory(s:state)
    execute 'set shadafile=' . fnameescape(s:state . '/shada/main.shada')
  endif
else
  if isdirectory(s:state)
    execute 'set viminfo+=n' . fnameescape(s:state . '/viminfo')
  endif
endif

" Share vim's pack directory with nvim
if s:is_nvim
  let s:vim_config = s:config_home . '/vim'
  if isdirectory(s:vim_config . '/pack')
    let s:rtp = split(&runtimepath, ',')
    let s:insert_pos = 1
    call insert(s:rtp, s:vim_config, s:insert_pos)
    if isdirectory(s:vim_config . '/after') && index(s:rtp, s:vim_config . '/after') < 0
      call add(s:rtp, s:vim_config . '/after')
    endif
    let &runtimepath = join(s:rtp, ',')
    let s:pp = split(&packpath, ',')
    if index(s:pp, s:vim_config) < 0
      call insert(s:pp, s:vim_config, 0)
    endif
    let &packpath = join(s:pp, ',')
  endif
endif
