#!/usr/bin/perl

use strict;
use File::Find;

sub clean {
  my $file = $_;
  if ($file =~ /\.hi$/ || $file =~ /\.o$/) {
    print "Cleaning $File::Find::name\n";
    unlink $file || die "Could not clean $file";
  }
}

find(\&clean, ("."));

print "Directory Cleaned\n";

