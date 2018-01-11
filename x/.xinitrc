#!/usr/bin/env zsh
setxkbmap gb
xrdb -merge ~/.Xresources
xset -b
xset +fp /usr/local/lib/X11/fonts/terminus/
xset +fp /usr/local/share/fonts/terminus
xset +fp /usr/share/fonts/terminus
xsetroot -cursor_name left_ptr
xsetroot -solid "#101010"
xbacklight -set 100
(while true; do xsetroot -name "$(date +'%a %e %b %Y %H:%M:%S')"; sleep 1; done) &
# if we have private ssh key(s), start ssh-agent and add the key(s)
id1=$HOME/.ssh/identity
id2=$HOME/.ssh/id_dsa
id3=$HOME/.ssh/id_rsa
id4=$HOME/.ssh/id_ecdsa
id5=$HOME/.ssh/id_ed25519
if [ -z "$SSH_AGENT_PID" ];
then
        if [ -x /usr/bin/ssh-agent ] && [ -f $id1 -o -f $id2 -o -f $id3 -o -f $id4 -o -f $id5 ];
        then
                eval `ssh-agent -s`
                ssh-add < /dev/null
        fi
fi
dwm

