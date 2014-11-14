#!/usr/bin/env perl
use strict;
use warnings;
use Getopt::Std;
$Getopt::Std::STANDARD_HELP_VERSION = 1;

sub HELP_MESSAGE {
    print STDERR <<EOF;

  -h, --help        this help message
  -v                be more verbose

Report bugs to <kaashif\@kaashif.co.uk>
EOF
    exit 0;
}

sub VERSION_MESSAGE {
    print STDERR "updatedns 0.0.l\n";
}

my %opts;
getopts('hv', \%opts);
if ($opts{h}) {
    VERSION_MESSAGE();
    HELP_MESSAGE();
}
my $verbose = $opts{v};
my $update_url = "http://dynamicdns.park-your-domain.com/update?domain=kaashif.co.uk&password=b7bb0f8c0c084db59babc0e5b1dc9fdf&host=";
my @subdomains = ("www", "@", "files", "git", "ftp");
my $has_failed = 0;

foreach (@subdomains) {
    my $next_url = "$update_url$_";
    print("Updating using URL: $next_url\n") if $verbose;
    my $xml_output = `curl -s "$next_url"`;
    print "$xml_output\n" if $verbose;
    if ($xml_output =~ /<ErrCount>0/) {
        print("DNS update of subdomain $_ succeeded!\n\n") if $verbose;
    } else {
        print("DNS update of subdomain $_ failed!\n\n") if $verbose;
        $has_failed = 1;
    }
}

exit $has_failed;
