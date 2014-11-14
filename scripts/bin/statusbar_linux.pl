#!/usr/bin/env perl
use warnings;
use strict;
use File::Slurp;
use Time::Piece;
our $home = $ENV{HOME};
sub music {
	my $mpc = `MPD_HOST=127.0.0.1 mpc 2>&1`;
	if ($mpc =~ /error/){
		return("^fg(#666666)^i($home/.xbm/note.xbm) ^fg(#999999)Please start MPD  ");
	}
	my @lines = split("\n", $mpc);
	my $title = $lines[0];
	my (@times, $time);
	if ($title =~ /consume:/ and $title =~ /single/) {
		$time = "";
		$title = "Ready";
	} else {
		@times = split(" ", $lines[1]);
		$time = $times[2];
	}
	return("^fg(#666666)^i($home/.xbm/note.xbm) ^fg(#999999)$title $time  ");
}
sub battery {
	my $raw = `acpi`;
	my @fields = split(": ", $raw);
	@fields = split(" ", $fields[1]);
	my $bat_left = ($fields[1] =~ s/%//g);
	my $bat_icon;
	if ($bat_left <= 50) {
		$bat_icon = "^i($home/.xbm/bat_empty_01.xbm)";
	} elsif ($bat_left <= 95) {
		$bat_icon = "^i($home/.xbm/bat_low_01.xbm)";
	} else {
		$bat_icon = "^i($home/.xbm/bat_full_01.xbm)";
	}
	return(sprintf("^fg(#666666)$bat_icon ^fg(#999999)%d%  ", $bat_left));
}
sub ram {
	my $mem = `top -d1`;
	my @mems = split("\n", $mem);
	my @free_ms = split(" ", $mems[3]);
	my $free_m = $free_ms[1];
	return("^fg(#666666)^i($home/.xbm/mem.xbm) ^fg(#999999)${free_m}  ");
}
sub temp {
	my @raw = split("\n", `sensors acpitz-virtual-0 -u`);
	my $temp;
	foreach (@raw) {
		if (/^  temp1_input/) {
			s/temp1//g;
			s/[^0-9.]//g;
			$temp = $_;
			my $tempicon = "$home/.xbm/temp.xbm";
			return(sprintf("^fg(#666666)^i($tempicon) ^fg(#999999)%1.1f°C  ", $temp));
		}
	}
}
sub wifi {
	my @raw = split("\n",`iwconfig wlan0`);
	my $network;
	foreach (@raw) {
		if (/^wlan0/) {
			my @networks = split(":", $_);
			$network = ($networks[1] =~ s/\"//g);
			return("^fg(#666666)^i($home/.xbm/wifi_01.xbm) ^fg(#999999)$network  ");
		}
	}
}
sub load {
	my $load = `uptime`;
	chomp($load);
	my @loads = split("average: ", $load);
	return("^fg(#666666)^i($home/.xbm/cpu.xbm) ^fg(#999999)$loads[1]  ");
}
sub date {
	my $datetime = localtime->strftime("%a %d-%m-%Y %H:%M:%S");
	return("^fg(#666666)^i($home/.xbm/clock.xbm) ^fg(#999999)$datetime ");
}
sub volume {
	my @rawvol = split("\n", `amixer get Master`);
	my @vols = split(' ', $rawvol[4]);
	my $vol = $vols[3];
	$vol =~ s/[\[\]]//g;
	chomp($vol);
	return("^fg(#666666)^i($home/.xbm/spkr_01.xbm) ^fg(#999999)$vol  ");
}

while() {
	printf("%s%s%s%s%s%s%s\n", music(), volume(), temp(), load(), date());
	sleep(1);
}
