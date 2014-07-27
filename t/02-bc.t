
use strict;
use warnings;

use Test::More tests => 1;
use Expect;

subtest raw_pty_bc => sub {
	my $bc = '/usr/bin/bc';
	if ( not -x $bc ) {
		plan skip_all => "Need to have $bc installed to run this test";
	}

	plan tests => 1;

	#$Expect::Debug = 1;

	my $e = Expect->new;
	$e->raw_pty(1);

	$e->spawn("bc") or die "Cannot run bc\n";
	$e->expect( 1, [qr/warranty'\./] ) or die "no warranty\n";
	$e->send("23+7\n");
	$e->expect( 1, [qr/\d+/] ) or die "no sum\n";
	my $match = $e->match;
	is $match, 30;
	$e->send("quit\n");
};

