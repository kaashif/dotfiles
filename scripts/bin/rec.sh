#!/bin/sh
recwindow -g &
lastpid=$!
sleep $1
kill -2 $lastpid
