#!/usr/bin/env zsh

song=`mpc list title | dmenu -w 1280 -nb '#101010' -nf '#999999' -sf '#666666' -sb '#101010' -fn 'Terminus:size=8' -p "Play:"`
if [[ -z "$song" ]]; then
	exit 0
else
	mpc playlist -f "%position% %title%" | grep -i "$song" | head -n1 | cut -d' ' -f1 | xargs mpc play
fi
