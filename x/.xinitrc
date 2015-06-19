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
export MPD_HOST=127.0.0.1
source ~/.zprofile
(sxhkd &) &
(emacs --daemon &) &
exec xmonad
