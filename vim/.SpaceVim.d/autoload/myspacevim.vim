function! myspacevim#before() abort
  set &t_Co=256
  set &t_ut='a'
  hi Normal     ctermbg=NONE guibg=NONE
  hi LineNr     ctermbg=NONE guibg=NONE
  hi SignColumn ctermbg=NONE guibg=NONE
endfunction

function! myspacevim#after() abort
  set &t_Co=255
  set &t_ut='b'
  hi Normal     ctermbg=NONE guibg=NONE
  hi LineNr     ctermbg=NONE guibg=NONE
  hi SignColumn ctermbg=NONE guibg=NONE
endfunction
