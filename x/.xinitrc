#!/usr/bin/env zsh
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/local/lib/X11/fonts/terminus/
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
xmodmap ~/.xmodmap
xfce4-panel --disable-wm-check &
feh --bg-scale /home/kaashif/etc/stumpwm/lisp.png
sbcl --load /home/kaashif/bin/startstump.lisp
