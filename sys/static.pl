my @pages = ( {hs => 'Start/One.hs', html => 'start/1.html'}
            );

foreach my $page (@pages) {
  print "Generating $page->{'hs'} - $page->{'html'}\n";
  `runghc Static/$page->{'hs'} > html/$page->{'html'}`;
}

