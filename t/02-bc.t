
use strict;
use warnings;

use Test::More tests => 2;
use Expect;

my $bc = '/usr/bin/bc';
if ( not -x $bc ) {
	diag "Could not find bc in $bc";
	my $which = `which bc`;
	diag "which $which";
	plan skip_all => "Need to have $bc installed to run this test";
}
my $bc_version = `$bc -v`;
diag "--------- bc version on $^O";
diag $bc_version;
diag '---------';
# just some notes:
# on $^O = 'solaris' and on 'freebsd' bc does not have any banner (the warranty stuff)
# on 'solaris' it also does not have a -v flag

subtest raw_pty_bc => sub {

	if ($^O =~ /^(openbsd|netbsd|freebsd|solaris|darwin)$/) {
		plan skip_all => "This test fails on \$^O == \$Config{'osname'} == '$^O'";
	}

	#if ($^O =~ /^(darwin)$/) {
	#	diag "This test will almost certainly fail on \$^O == \$Config{'osname'} == '$^O'. You can install the module skipping this test, but please report the failure.";
	#	#plan skip_all => "This test fails on $^O";
	#}

	plan tests => 3;

	my $e = Expect->new;
	$e->raw_pty(1);

	$e->spawn($bc) or die "Cannot run bc\n";
	my $warranty;
	$e->expect( 1, [qr/warranty'\./ => sub { $warranty = 1 } ] );
	ok $warranty, 'warranty found' or do {
		diag $e->before;
		return;
	};
	$e->send("23+7\n");
	my $num;
	$e->expect( 1, [qr/\d+/ => sub { $num = 1 }] );
	ok $num, 'number found' or do {
		diag $e->before;
		return;
	};
	my $match = $e->match;
	is $match, 30, 'the number';
	$e->send("quit\n");
};

subtest pty_bc => sub {
	plan tests => 4;

	my $e = Expect->new;

	$e->spawn($bc) or die "Cannot run bc\n";
	my $warranty;
	$e->expect( 1, [qr/warranty'\./ => sub { $warranty = 1 } ] );

	SKIP: {
		skip "No banner on $^O ", 1 if $^O =~ /^(freebsd|solaris)$/;
		ok $warranty, 'warranty found' or do {
			diag $e->before;
			return;
		};
	}

	$e->send("23+7\n");
	my $expr;
	$e->expect( 1, [qr/23\+7/ => sub { $expr = 1 }] );
	ok $expr, 'expression';

	my $num;
	$e->expect( 1, [qr/\d+/ => sub { $num = 1 }] );
	ok $num, 'number found' or do {
		diag $e->before;
		return;
	};
	my $match = $e->match;
	is $match, 30, 'the number';
	$e->send("quit\n");
}


