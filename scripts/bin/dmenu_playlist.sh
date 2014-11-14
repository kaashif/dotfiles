#!/usr/bin/env zsh

PLAYLIST=`mpc lsplaylists | dmenu -w 1280 -nb '#101010' -nf '#999999' -sf '#666666' -sb '#101010' -fn 'Terminus:size=8' -p "Play:" | cut -d')' -f1`
if [[ -z "$PLAYLIST" ]]; then
    exit 0
else
    mpc clear && mpc load "$PLAYLIST" && mpc play
fi
