#!/usr/bin/perl

my $arg = $ARGV[0];

# horrible horrible hacky trick to shorten a few common shell operations
exec 'sys/build.sh' if ($arg eq 'build');
exec 'tree -I dist' if ($arg eq 'tree');
exec 'git commit --all' if ($arg eq 'commit');
exec 'git push' if ($arg eq 'push');
exec 'git status' if ($arg eq 'status');

