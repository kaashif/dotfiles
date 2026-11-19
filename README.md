Dotfiles
========

Installation
------------
To install in the most convenient way, you need to install GNU Stow
first, then stow each of the directories containing configs you want to
use. For example, if you only wanted to use my zsh configs, do the
following:

    $ stow zsh

To remove the symlinks created by stow:

    $ stow -D zsh

Without stow, you must copy the files manually. Installing stow only
takes a few seconds, it's best to do that, especially considering it
depends on nothing but Perl, which is always already installed.

Note: Most of the configs might only work as-is on OpenBSD.

Vim
---

Install with `stow vim`. The vimrc enables syntax highlighting and shows
`[Markdown lists]` in the statusline when the Markdown list plugin is loaded.
In a list item, Enter indents a continuation line. Enter again on that empty
line starts the next item. In Normal mode, `gqip` reflows the current item
using Vim's `textwidth` setting.

Run the Vim tests from the repository root.

    vim -Nu NONE -i NONE -n -es -S tests/vim-markdown.vim
