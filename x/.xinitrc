#!/usr/bin/env zsh
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/local/lib/X11/fonts/terminus/
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
xmodmap ~/.xmodmap

exec ssh-agent sbcl --load /home/kaashif/bin/startstump.lisp

