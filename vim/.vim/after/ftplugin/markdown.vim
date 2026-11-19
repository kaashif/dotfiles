if exists('b:did_markdown_list_editing')
  finish
endif
let b:did_markdown_list_editing = 1

let s:list_prefix = '^\([ \t]*\)\%(\([-+*]\)\|\(\d\{1,9}\)\([.)]\)\)\([ \t]\+\)'
let s:fence_start = '\%(\%d96\{3,}\|\~\{3,}\)'
let s:block_start = '^\s*\%(#\{1,6}\%(\s\|$\)\|>\|[-=]\+\s*$\|\%([-*_]\s*\)\{3,}$\|' . s:fence_start . '\)'

function! s:InFence(lnum) abort
  let fence = ''
  for line in getline(1, a:lnum)
    if empty(fence)
      let line = substitute(line, s:list_prefix, '', '')
      let fence = matchstr(line, '^\s*\zs' . s:fence_start)
    elseif line =~# '^\s*' . escape(fence . fence[0], '~') . '*\s*$'
      let fence = ''
    endif
  endfor
  return !empty(fence)
endfunction

" Return the marker that owns this line and its text indentation.
function! s:ListItem(lnum) abort
  let lnum = a:lnum
  let continuation_indent = indent(lnum)
  while lnum > 0
    let line = getline(lnum)
    if line =~# '^\s*$' || line =~# s:block_start
      break
    endif
    let parts = matchlist(line, s:list_prefix)
    if !empty(parts)
      let width = strdisplaywidth(parts[0])
      if (lnum == a:lnum || continuation_indent < width + 4) && !s:InFence(lnum)
        return [lnum, width, parts]
      endif
      break
    endif
    let continuation_indent = max([continuation_indent, indent(lnum)])
    let lnum -= 1
  endwhile
  return []
endfunction

function! s:ListEnter() abort
  let lnum = line('.')
  let line = getline(lnum)
  if line =~# '^[ \t]\+$' && col('.') == strlen(line) + 1
    let item = s:ListItem(lnum - 1)
    if !empty(item) && strdisplaywidth(line) == item[1]
      let parts = item[2]
      let marker = empty(parts[3]) ? parts[2]
            \ : printf('%0*d', strlen(parts[3]), str2nr(parts[3], 10) + 1) . parts[4]
      return "\<C-U>" . parts[1] . marker . parts[5]
    endif
  endif

  let item = s:ListItem(lnum)
  if !empty(item) && (item[0] != lnum || col('.') > strlen(item[2][0]))
    " 0 followed by CTRL-D clears any automatic indent on the new line.
    return "\<CR>0\<C-D>" . repeat(' ', item[1])
  endif
  return "\<CR>"
endfunction

function! s:ReflowParagraph() abort
  let item = s:ListItem(line('.'))
  if empty(item)
    normal! gqip
    return
  endif

  let first = item[0]
  let width = item[1]
  let last = line('.')
  while last < line('$')
    let next = getline(last + 1)
    if next =~# '^\s*$' || next =~# s:list_prefix || next =~# s:block_start
          \ || indent(last + 1) >= width + 4
      break
    endif
    let last += 1
  endwhile
  " Markdown allows continuation text without the list item's indentation.
  let continuation = getline(first + 1, last)
  let aligned = map(copy(continuation), {_, text -> repeat(' ', width) . substitute(text, '^\s*', '', '')})
  if continuation !=# aligned
    call setline(first + 1, aligned)
    undojoin
  endif

  " Keep the hanging indent even when a single word exceeds textwidth.
  let autoindent = &l:autoindent
  setlocal autoindent
  try
    execute first . 'normal! V' . (last > first ? (last - first) . 'j' : '') . 'gq'
  finally
    let &l:autoindent = autoindent
  endtry
endfunction

" Extend Vim's Markdown list formatting to ordered markers ending in ')'.
let &l:formatlistpat = '^\s*\d\{1,9})[ \t]\+\|' . &l:formatlistpat
if &l:textwidth == 0
  setlocal textwidth=80
endif
setlocal autoindent formatoptions+=tn formatoptions-=l

inoremap <buffer> <CR> <Cmd>call feedkeys(<SID>ListEnter(), 'in')<CR>
nnoremap <buffer><silent> gqip <Cmd>call <SID>ReflowParagraph()<CR>

let b:undo_ftplugin .= '|setlocal autoindent< textwidth<|silent! iunmap <buffer> <CR>'
      \ . '|silent! nunmap <buffer> gqip|unlet! b:did_markdown_list_editing'
