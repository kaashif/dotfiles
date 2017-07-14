#!/usr/bin/env zsh
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/local/lib/X11/fonts/terminus/
xset +fp /usr/local/share/fonts/terminus
xset +fp /usr/share/fonts/terminus
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
feh --bg-scale /home/kaashif/etc/stumpwm/lisp.png
dwm

