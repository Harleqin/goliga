#!/usr/bin/perl
use strict;
use warnings;
use Data::Dumper;

use Graph::Matching qw(max_weight_matching);

#if ($ARGV[0] =~ m/[^\[\],0-9\.()]/) {die "invalid input"}

my $graph;
eval join (' ', '$graph = ', $ARGV[0]);

my %result = max_weight_matching ($graph, 1);

my %seen;

foreach my $k (keys %result) {
    unless ($seen{$k}) {print $k, ",", $result{$k}, "\n";}
    $seen{$result{$k}} = 1;
}
