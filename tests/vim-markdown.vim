let s:vim_dir = expand('<sfile>:p:h:h') . '/vim'
let &runtimepath = join([s:vim_dir . '/.vim', $VIMRUNTIME, s:vim_dir . '/.vim/after'], ',')
execute 'source ' . fnameescape(s:vim_dir . '/.vimrc')

execute 'edit ' . fnameescape(tempname() . '.md')
call assert_equal('markdown', &l:filetype)
call assert_equal(80, &l:textwidth)
call assert_equal('markdown', b:current_syntax)
call setline(1, '# A heading')
call assert_equal('markdownH1', synIDattr(synID(1, 3, 1), 'name'))
call assert_equal(2, &laststatus)
let s:status_indicator = matchstr(&statusline, '%{\zs.\{-}\ze}')
call assert_equal(' [Markdown lists]', eval(s:status_indicator))

function! s:CheckEdit(input, lnum, keys, expected) abort
  silent %delete _
  call setline(1, a:input)
  call cursor(a:lnum, 1)
  call feedkeys(a:keys, 'xt')
  call assert_equal(a:expected, getline(1, '$'), string(a:keys) . ' on line ' . a:lnum)
endfunction

call s:CheckEdit(['- Pack the tent'], 1, "A\<CR>Bring stakes\<Esc>",
      \ ['- Pack the tent', '  Bring stakes'])
call s:CheckEdit(['- Pack the tent'], 1, "A\<CR>\<CR>Bring stakes\<Esc>",
      \ ['- Pack the tent', '- Bring stakes'])
call s:CheckEdit(['- Supplies', '  12. Pack the tent'], 2, "A\<CR>Bring stakes\<CR>\<CR>Pack food\<Esc>",
      \ ['- Supplies', '  12. Pack the tent', '      Bring stakes', '  13. Pack food'])
call s:CheckEdit(['   09) Pack the tent'], 1, "A\<CR>\<CR>Bring stakes\<Esc>",
      \ ['   09) Pack the tent', '   10) Bring stakes'])

let s:fence = repeat(nr2char(96), 3)
call s:CheckEdit([s:fence, '- Shell command', s:fence], 2, "A\<CR>More\<Esc>",
      \ [s:fence, '- Shell command', 'More', s:fence])

setlocal textwidth=30
call s:CheckEdit([''], 1, "iPack the tent and bring enough stakes for the group.\<Esc>",
      \ ['Pack the tent and bring enough', 'stakes for the group.'])
call s:CheckEdit(['  12. Pack the tent'], 1, "A and bring enough stakes for the group.\<Esc>",
      \ ['  12. Pack the tent and bring', '      enough stakes for the', '      group.'])
call s:CheckEdit(['  - Pack the tent and bring enough stakes'], 1, "A for the group.\<Esc>",
      \ ['  - Pack the tent and bring', '    enough stakes for the', '    group.'])

setlocal textwidth=40 noautoindent
call s:CheckEdit(['  12. Check the tent poles and pack the repair kit before leaving.', '  13. Fill the water bottles.'],
      \ 1, 'gqip', ['  12. Check the tent poles and pack the', '      repair kit before leaving.', '  13. Fill the water bottles.'])
call s:CheckEdit(['- Check the tent poles and pack the repair kit before leaving.', '  - Check the matches.'],
      \ 1, 'gqip', ['- Check the tent poles and pack the', '  repair kit before leaving.', '  - Check the matches.'])

" The long link and the unindented lines belong to the first list item.
let s:link = '[details](https://example.com/a-url-that-is-longer-than-the-configured-textwidth)'
let s:next_paragraph = ['2. Bring water.', '', 'A separate paragraph.']
let s:input = ['1. Read the gear checklist and', '   review the', s:link, 'Then pack the repair kit.'] + s:next_paragraph
let s:expected = ['1. Read the gear checklist and review', '   the', '   ' . s:link, '   Then pack the repair kit.'] + s:next_paragraph
for s:lnum in range(1, 4)
  call s:CheckEdit(s:input, s:lnum, 'gqip', s:expected)
  call assert_false(&l:autoindent)
endfor

let s:front_matter = ['---', 'title: A long title that should be kept on its own line', '---', '']
let s:unchanged_blocks = [
      \ '', 'This long Setext title should stay on a single line', '===', '',
      \ s:fence . 'sh', '- A command inside the fence that should stay on one line', s:fence, '',
      \ 'Name | A nice long table heading that should stay on one line', '--- | ---',
      \ 'Tent | A long table cell should also stay on its own line', '',
      \ '    Indented code should also stay exactly as it is.', '',
      \ 'This long line marks a hard break and should stay on one line.  ', 'The next line.', '']
let s:whole_file = s:front_matter
      \ + ['Pack the tent and bring enough stakes for the entire group.', '']
      \ + s:input
      \ + ['  - Check that the tent poles and repair kit are packed.']
      \ + s:unchanged_blocks
      \ + ['Fill the water bottles and make sure we have enough for the whole trip.']
let s:whole_file_expected = s:front_matter
      \ + ['Pack the tent and bring enough stakes', 'for the entire group.', '']
      \ + s:expected
      \ + ['  - Check that the tent poles and repair', '    kit are packed.']
      \ + s:unchanged_blocks
      \ + ['Fill the water bottles and make sure we', 'have enough for the whole trip.']
call s:CheckEdit(s:whole_file, 3, 'gg=G', s:whole_file_expected)
silent undo
call assert_equal(s:whole_file, getline(1, '$'), 'one undo reverses the whole-buffer reflow')
call feedkeys('gg=G', 'xt')
call feedkeys('gg=G', 'xt')
call assert_equal(s:whole_file_expected, getline(1, '$'), 'whole-buffer reflow is idempotent')

setlocal filetype=text
call assert_equal(0, &l:textwidth)
call assert_equal('', maparg('<CR>', 'i'))
call assert_equal('', maparg('gqip', 'n'))
call assert_equal('', maparg('gg=G', 'n'))
call assert_equal('', eval(s:status_indicator))
setlocal textwidth=72 filetype=markdown
call assert_equal(72, &l:textwidth)
call assert_equal(' [Markdown lists]', eval(s:status_indicator))

let s:draft = tempname() . '.md.draft'
execute 'edit! ' . fnameescape(s:draft)
call assert_equal('markdown', &l:filetype, 'new draft file')
call assert_equal('markdown', get(b:, 'current_syntax', ''), 'new draft syntax')
call assert_equal(' [Markdown lists]', eval(s:status_indicator))

try
  call writefile(s:whole_file, s:draft)
  bwipeout!
  execute 'edit ' . fnameescape(s:draft)
  call assert_equal('markdown', &l:filetype, 'existing draft file')
  setlocal textwidth=40
  call feedkeys('gg=G', 'xt')
  call assert_equal(s:whole_file_expected, getline(1, '$'), 'whole-buffer reflow in a draft')
finally
  call delete(s:draft)
endtry

if !empty(v:errors)
  call writefile(v:errors, '/dev/stderr')
  cquit
endif
qa!
