filetype plugin on
syntax on

set laststatus=2
let &statusline = '%<%f %h%w%m%r'
      \ . "%{get(b:, 'did_markdown_list_editing', 0) ? ' [Markdown lists]' : ''}"
      \ . '%=%-14.(%l,%c%V%) %P'
