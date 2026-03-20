let s:vim_config = expand('~/.config/vim')

if isdirectory(s:vim_config)
	let &packpath = s:vim_config . ',' . &packpath
	let &runtimepath = s:vim_config . ',' . &runtimepath
	if isdirectory(s:vim_config . '/after')
		let &runtimepath .= ',' . s:vim_config . '/after'
	endif
endif
