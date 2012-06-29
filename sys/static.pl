my @pages = ( {hs => 'Start/One.hs', html => 'start/1.html'}
            );

foreach my $page (@pages) {
  print "Generating $page->{'hs'} - $page->{'html'}\n";
  print `runghc Static/$page->{'hs'} | tee html/$page->{'html'}`;
  print "\n\n\n";
}

