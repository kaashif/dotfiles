#!/usr/bin/env zsh

hosts="$(cat .ssh/config | grep -E ^Host | cut -d' ' -f2)"
selected="$(echo $hosts| dmenu -w 1280 -nb '#101010' -nf '#999999' -sf '#666666' -sb '#101010' -fn 'Terminus:size=8' -p "ssh to:" | cut -d')' -f1)"

if [[ -z "$selected" ]]; then
	exit
fi

urxvt -e zsh -c "TERM=xterm-color ssh $selected"
