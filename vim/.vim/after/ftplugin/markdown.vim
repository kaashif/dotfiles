if exists('b:did_markdown_list_editing')
  finish
endif
let b:did_markdown_list_editing = 1

let s:list_prefix = '^\([ \t]*\)\%(\([-+*]\)\|\(\d\{1,9}\)\([.)]\)\)\([ \t]\+\)'
let s:fence_start = '\%(\%d96\{3,}\|\~\{3,}\)'
let s:block_start = '^\s*\%(#\{1,6}\%(\s\|$\)\|>\|[-=]\+\s*$\|\%([-*_]\s*\)\{3,}$\|' . s:fence_start . '\)'
let s:protected_line = '^\s*\%($\|<\|\[[^]]\+\]:\)\||'

function! s:NextFence(line, fence) abort
  if empty(a:fence)
    let text = substitute(a:line, s:list_prefix, '', '')
    return matchstr(text, '^\s*\zs' . s:fence_start)
  endif
  return a:line =~# '^\s*' . escape(a:fence . a:fence[0], '~') . '*\s*$' ? '' : a:fence
endfunction

function! s:InFence(lnum) abort
  let fence = ''
  for text in getline(1, a:lnum)
    let fence = s:NextFence(text, fence)
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

function! s:ParagraphEnd(start, code_indent, stop_pattern) abort
  let last = a:start
  while last < line('$')
    let next = getline(last + 1)
    if next =~# a:stop_pattern || next =~# s:list_prefix || next =~# s:block_start
          \ || indent(last + 1) >= a:code_indent
      break
    endif
    let last += 1
  endwhile
  return last
endfunction

function! s:ReflowRange(first, last, list_width) abort
  if a:list_width > 0
    " Markdown allows continuation text without the list item's indentation.
    let continuation = getline(a:first + 1, a:last)
    let prefix = repeat(' ', a:list_width)
    let aligned = map(copy(continuation), {_, text -> prefix . substitute(text, '^\s*', '', '')})
    if continuation !=# aligned
      call setline(a:first + 1, aligned)
      undojoin
    endif
  endif

  " Keep the hanging indent even when a single word exceeds textwidth.
  let autoindent = &l:autoindent
  setlocal autoindent
  try
    execute 'keepjumps ' . a:first . 'normal! V' . (a:last > a:first ? (a:last - a:first) . 'j' : '') . 'gq'
  finally
    let &l:autoindent = autoindent
  endtry
endfunction

function! s:ReflowParagraph() abort
  let item = s:ListItem(line('.'))
  if empty(item)
    normal! gqip
    return
  endif
  let first = item[0]
  let width = item[1]
  call s:ReflowRange(first, s:ParagraphEnd(line('.'), width + 4, '^\s*$'), width)
endfunction

function! s:ParagraphRanges() abort
  let ranges = []
  let fence = ''
  let list_widths = []
  let lnum = 1
  if getline(1) =~# '^\%(---\|+++\)\s*$'
    let closing = match(getline(2, '$'), '^' . trim(getline(1)) . '\s*$')
    if closing >= 0
      let lnum = closing + 3
    endif
  endif

  while lnum <= line('$')
    let text = getline(lnum)
    let in_fence = !empty(fence)
    let fence = s:NextFence(text, fence)
    if in_fence || !empty(fence) || text =~# s:protected_line || text =~# s:block_start
      let lnum += 1
      continue
    endif

    let indent = indent(lnum)
    while !empty(list_widths) && indent < list_widths[-1]
      call remove(list_widths, -1)
    endwhile
    let base = empty(list_widths) ? 0 : list_widths[-1]
    if indent >= base + 4
      let lnum += 1
      continue
    endif

    let marker = matchstr(text, s:list_prefix)
    let width = strdisplaywidth(marker)
    if width > 0
      call add(list_widths, width)
    endif
    let last = s:ParagraphEnd(lnum, (width > 0 ? width : base) + 4, s:protected_line)
    let setext_heading = width == 0 && getline(last + 1) =~# '^\s*\%(-\+\|=\+\)\s*$'
    let hard_break = match(getline(lnum, last), '\%(  \+\|\\\)$') >= 0
    if !setext_heading && !hard_break
      call add(ranges, [lnum, last, width])
    endif
    let lnum = last + 1
  endwhile
  return ranges
endfunction

function! s:ReflowBuffer() abort
  let view = winsaveview()
  let foldenable = &l:foldenable
  let initial_tick = b:changedtick
  setlocal nofoldenable
  try
    " Work upward so changes do not shift the remaining ranges.
    for [first, last, width] in reverse(s:ParagraphRanges())
      if b:changedtick != initial_tick
        undojoin
      endif
      call s:ReflowRange(first, last, width)
    endfor
  finally
    let &l:foldenable = foldenable
    call winrestview(view)
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
nnoremap <buffer><silent> gg=G <Cmd>call <SID>ReflowBuffer()<CR>

let b:undo_ftplugin .= '|setlocal autoindent< textwidth<|silent! iunmap <buffer> <CR>'
      \ . '|silent! nunmap <buffer> gqip|silent! nunmap <buffer> gg=G'
      \ . '|unlet! b:did_markdown_list_editing'
