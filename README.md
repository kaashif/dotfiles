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
