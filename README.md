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

Portability
-----------
Currently, I'm using them on OpenBSD, but they do work on FreeBSD and
most flavours of GNU/Linux. The only thing that would probably need to
be changed is the statusbar.pl script, which pulls info from various
OS-specific sysctls and hardware sensors.

If you're on OpenBSD, [hinfo](http://github.com/kaashif-hymabaccus/hinfo)
will also work, meaning you can probably install the dotfiles and use
them as-is, no Perl hacking required.

The xmonad config will also need to be changed if you're not on OpenBSD. If
you don't, the volume controls won't work.
