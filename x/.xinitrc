#!/usr/bin/env zsh
export LC_CTYPE="en_GB.UTF-8"
export LC_ALL="en_GB.UTF-8"
export LC_MESSAGES="en_GB.UTF-8"
export LANG="en_GB.UTF-8"
export XDG_DESKTOP_DIR="$HOME"
export XDG_DOWNLOAD_DIR="$HOME"
export DISPLAY=":0"
source $HOME/.zprofile
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/share/fonts/misc
xset +fp /usr/share/fonts/local
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
export MPD_HOST=127.0.0.1
source ~/.zprofile
(sxhkd &) &
mpd
eval $(ssh-agent -a /tmp/ssh-agent.socket)
ssh-add
exec xmonad
