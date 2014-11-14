#!/usr/bin/env zsh
(while true; do sleep 1 && /home/kaashif/.cabal/bin/hinfo -iF "music volume temp wifi load time" | cat; done) | dzen2 -dock -h "16" -x "0" -y "0" -w "1280" -bg "#101010" -ta r -fn '-*-terminus-medium-r-*--*-93' &

