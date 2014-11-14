#!/usr/bin/env zsh
export LC_CTYPE="en_GB.UTF-8"
export LC_ALL="en_GB.UTF-8"
export LC_MESSAGES="en_GB.UTF-8"
export LANG="en_GB.UTF-8"
export XDG_DESKTOP_DIR="$HOME"
export XDG_DOWNLOAD_DIR="$HOME"
export DISPLAY=":0"
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
exec ~/.cabal/bin/xmonad
