#!/usr/bin/perl
use strict;
use warnings;

while (<>) {
    if (/Podcast/) {
        my @f = split(' ', $_);
        @f = grep(/^http/, @f);
        my $url = $f[0];
        @f = split('/', $url);
        my $filename = $f[-1];
        print "filename: $filename\n";
        print "url: $url\n";
        chdir($ENV{HOME} . "/tmp");
        system("ftp", "$url");
    }
}
