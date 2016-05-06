#!/bin/sh
ifconfig urtwn0 inet -inet6 \
		 media autoselect \
		 -bssid \
		 -chan \
		 -nwkey \
		 nwid "eduroam" \
		 wpa \
		 wpaprotos wpa2 \
		 wpaakms 802.1x \
		 wpaciphers ccmp \
		 wpagroupcipher ccmp \
		 up
rcctl start wpa_supplicant

echo Sleeping to wait for authentication to finish...
sleep 10

dhclient urtwn0

