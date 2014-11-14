#!/usr/bin/env perl
use warnings;
use strict;
use File::Slurp;
use Time::Piece;
our $home = $ENV{HOME};
sub music {
	my $mpc = `mpc 2>&1`;
	if ($mpc =~ /error: Connection refused/){
		return("^fg(#666666)^i($home/.xbm/note.xbm) ^fg(#999999)Please start MPD  ");
	}
	my @lines = split("\n", $mpc);
	my $title = $lines[0];
	my @times = split(" ", $lines[1]);
	my $time = $times[2];
	if ($title =~ /consume:/ and $title =~ /single/) {
		$time = "";
		$title = "Ready";
	}
	return("^fg(#666666)^i($home/.xbm/note.xbm) ^fg(#999999)$title $time  ");
}
sub battery {
	my @lastfull = split(" ", `sysctl -n hw.sensors.acpibat0.watthour0`);
	my @current = split(" ", `sysctl -n hw.sensors.acpibat0.watthour3`);
	my @rate = split(" ", `sysctl -n hw.sensors.acpibat0.power0`);
	return("") unless (@lastfull) and (@current) and (@rate);
	my $time_left;
	if ($rate[0] == 0) {
		$time_left = "unknown";
	} else {
		$time_left = $current[0]/$rate[0];
		my $hours_left = int($time_left);
		my $mins_left = sprintf("%.2f", ($time_left - $hours_left))*60;
		$time_left = sprintf("%d hour(s), %d minute(s)", $hours_left, $mins_left);
	}
	my $bat_left = 100*$current[0]/$lastfull[0];
	my $bat_icon;
	if ($bat_left <= 50) {
		$bat_icon = "^i($home/.xbm/bat_empty_01.xbm)";
	} elsif ($bat_left <= 95) {
		$bat_icon = "^i($home/.xbm/bat_low_01.xbm)";
	} else {
		$bat_icon = "^i($home/.xbm/bat_full_01.xbm)";
	}
	return(sprintf("^fg(#666666)$bat_icon ^fg(#999999)%.1f%% : $time_left  ", $bat_left));
}
sub ram {
	my $mem = `top -d1`;
	my @mems = split("\n", $mem);
	my @free_ms = split(" ", $mems[3]);
	my $free_m = $free_ms[1];
	return("^fg(#666666)^i($home/.xbm/mem.xbm) ^fg(#999999)${free_m}  ");
}
sub temp {
	my $sysctl_temp = `sysctl hw.sensors.cpu0.temp0`;
	my @temps =  split("=", $sysctl_temp);
	my $temp = $temps[1];
	chomp($temp);
	my $tempicon = "$home/.xbm/temp.xbm";
	return("^fg(#666666)^i($tempicon) ^fg(#999999)${temp}  ");
}
sub wifi {
	my $info = `ifconfig iwn0`;
	my @infos = split("\n", $info);
	my @networks = split(" ", $infos[6]);
	my $network = $networks[2];
	$network =~ s/ssid//;
	$network =~ s/\"//g;
	return("^fg(#666666)^i($home/.xbm/wifi_01.xbm) ^fg(#999999)$network  ");
}
sub load {
	my $load = `uptime`;
	chomp($load);
	my @loads = split("averages: ", $load);
	return("^fg(#666666)^i($home/.xbm/cpu.xbm) ^fg(#999999)$loads[1]  ");
}
sub date {
	my $datetime = localtime->strftime("%a %d-%m-%Y %H:%M:%S");
	return("^fg(#666666)^i($home/.xbm/clock.xbm) ^fg(#999999)$datetime ");
}
sub volume {
	my $rawvol = `mixerctl outputs.master`;
	my @vols = split('=', $rawvol);
	my $vol = $vols[1];
	chomp($vol);
	return("^fg(#666666)^i($home/.xbm/spkr_01.xbm) ^fg(#999999)$vol  ");
}

while() {
	printf("%s%s%s%s%s%s%s\n", music(), battery(), volume(), temp(), wifi(), load(), date());
	sleep(1);
}
