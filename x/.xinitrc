#!/usr/bin/env zsh
source ~/.zprofile
zsh $HOME/.zshrc no
eval $(ssh-agent)
ssh-add
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/local/lib/X11/fonts/terminus/
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
(sxhkd &) &
(emacs --daemon &) &
panel.sh
nmgui &
exec xmonad
