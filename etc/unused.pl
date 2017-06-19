#!/usr/bin/perl

use strict;
use warnings;
use 5.010;

use Data::Dumper;

my $fh;
my @alpha;

open($fh, '<', './Shiny/Case.hs');
for (<$fh>) {
    s/\r\n/\n/;
    chomp;
    @alpha = split //, $1 if /^lowerCase = "([^"]*)"$/
}
close($fh);

my %alpha;

@alpha{@alpha} = ();
@alpha{ map { my $x = $_; map $x . $_, @alpha } @alpha } = ();

open($fh, '<', './Shiny/Standard.hs');
for (<$fh>) {
    $alpha{$1}++ if /"([^"]{0,2})"/;
}
close($fh);

for (keys %alpha) {
    print "$_\n" unless $alpha{$_};
}
