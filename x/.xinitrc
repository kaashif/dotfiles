#!/usr/bin/env zsh
#eval $(ssh-agent)
#ssh-add
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/local/lib/X11/fonts/terminus/
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
xmodmap ~/.xmodmap
#(sxhkd &) &
#panel.sh
#stalonetray -i 16 &
#nmgui &
exec sbcl --load /home/kaashif/bin/startstump.lisp
#exec xmonad


