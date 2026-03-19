let s:config_home = empty($XDG_CONFIG_HOME) ? expand('~/.config')      : $XDG_CONFIG_HOME
let s:data_home   = empty($XDG_DATA_HOME)   ? expand('~/.local/share') : $XDG_DATA_HOME
let s:state_home  = empty($XDG_STATE_HOME)  ? expand('~/.local/state') : $XDG_STATE_HOME
let s:cache_home  = empty($XDG_CACHE_HOME)  ? expand('~/.cache')       : $XDG_CACHE_HOME

let s:config = s:config_home . '/vim'
let s:data   = s:data_home   . '/vim'
let s:state  = s:state_home  . '/vim'
let s:cache  = s:cache_home  . '/vim'

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
  execute 'set directory=' . s:cache . '/swap//,.,/tmp'
endif

if isdirectory(s:cache . '/backup')
  execute 'set backupdir=' . s:cache . '/backup//,.,/tmp'
endif

if isdirectory(s:data . '/undo')
  execute 'set undodir=' . s:data . '/undo//'
endif

if isdirectory(s:data . '/spell')
  execute 'set spellfile=' . s:data . '/spell/en.utf-8.add'
endif

if isdirectory(s:state . '/view')
  execute 'set viewdir=' . s:state . '/view/'
endif

if isdirectory(s:state)
  execute 'set viminfo+=n' . s:state . '/viminfo'
endif
