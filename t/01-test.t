use strict;
use warnings;

use Test::More tests => 12;
use File::Temp qw(tempdir);
use Expect;

#$Expect::Exp_Internal = 1;
#$Expect::Debug = 1;

my $tempdir = tempdir( CLEANUP => 1 );
my $Perl = $^X;

subtest perl => sub {
	diag "Basic tests...";
	plan tests => 4;

	my $exp = Expect->spawn("$Perl -v");
	ok( defined $exp );
	$exp->log_user(0);
	is( $exp->expect( 10, "krzlbrtz",   "Copyright" ), 2 );
	is( $exp->expect( 10, "Larry Wall", "krzlbrtz" ),  1 );
	ok( not $exp->expect( 3, "Copyright" ) );
};


subtest exec_failure => sub {
	diag "Testing exec failure...";
	plan tests => 6;

	my $exp = Expect->new;
	ok( defined $exp );
	$exp->log_stdout(0);
	$! = 0;
	ok( not defined $exp->spawn("Ignore_This_Error_Its_A_Test__efluna3w6868tn8") );
	ok($!);
	my $val = '';
	my $res = $exp->expect(
		20,
		[ "Cannot exec" => sub { $val = 'cannot_exec'; } ],
		[ eof           => sub { $val = 'eof'; } ],
		[ timeout       => sub { $val = 'timeout'; } ],
	);
	is $val, 'cannot_exec';
	ok( defined $res );
	is( $res, 1 );
};


subtest exp_continue => sub {
	diag "Testing exp_continue...";
	plan tests => 1;

	my $exp   = Expect->new( $Perl . q{ -e 'foreach (qw(A B C D End)) { print "$_\n"; }' } );
	my $state = "A";
	my @val;
	$exp->expect(
		2,
		[   "[ABCD]" => sub {
				my $self = shift;
				push @val, $self->match;
				exp_continue;
			}
		],
		[ "End"   => sub { push @val, 'End'; } ],
		[ eof     => sub { push @val, 'eof'; } ],
		[ timeout => sub { push @val, 'timeout'; } ],
	);
	is_deeply \@val, [qw(A B C D End)], '5 states of exp_continue';
	$exp->hard_close();
};

subtest exp_continue_sleep => sub {
	plan tests => 5;

	my $exp = Expect->new( $Perl . q{ -e 'print "Begin\n"; sleep (5); print "End\n";' } );
	my $cnt = 0;
	my ( $begin, $end, $eof );
	$exp->expect(
		1,
		[ "Begin" => sub { $begin = 1; exp_continue; } ],
		[ "End"   => sub { $end   = 1; } ],
		[ eof     => sub { $eof   = 1; } ],
		[ timeout => sub { $cnt++; ( $cnt < 7 ) ? exp_continue : 0; } ],
	);
	ok $begin;
	ok $end;
	ok !$eof;
	diag "number of timeout calls in 5 sec: $cnt";
	cmp_ok( $cnt, '>', 2 );
	cmp_ok( $cnt, '<', 7 );
	$exp->hard_close();
};

subtest timeout => sub {
	diag "timeout shouldn't destroy accum contents";
	plan tests => 3;

	my $exp = Expect->new( $Perl . q{ -e 'print "some string\n"; sleep (5);' } );
	ok( not defined $exp->expect( 1, "NoMaTcH" ) );
	my $i = $exp->expect( 1, '-re', 'some\s' );
	ok( defined $i );
	is $i, 1;
	$exp->hard_close();
};


subtest notransfer => sub {
	diag "Testing -notransfer...";
	plan tests => 6;

	my $exp = Expect->new( $Perl . q{ -e 'print "X some other\n"; sleep 5;'} );
	$exp->notransfer(1);

	my @expected = ( 'some', 'some', 'other' );
	foreach my $e (@expected) {
		my $val = '';
		$exp->expect(
			3,
			[ $e      => sub { $val = $e; } ],
			[ eof     => sub { $val = 'eof'; } ],
			[ timeout => sub { $val = 'timeout'; } ],
		);
		is $val, $e;
	}

	sleep(6);
	my $val1 = '';
	$exp->expect(
		3,
		[ 'some'  => sub { my $self = shift; $val1 = 'some'; $self->set_accum( $self->after() ); } ],
		[ eof     => sub { $val1 = 'eof'; } ],
		[ timeout => sub { $val1 = 'timeout'; } ],
	);
	is $val1, 'some';

	my $val2 = '';
	$exp->expect(
		3,
		[ 'some'  => sub { $val2 = 'some'; } ],
		[ 'other' => sub { $val2 = 'other'; my $self = shift; $self->set_accum( $self->after() ); } ],
		[ eof     => sub { $val2 = 'eof'; } ],
		[ timeout => sub { $val2 = 'timeout'; } ],
	);
	is $val2, 'other';

	my $val3 = '';
	$exp->expect(
		3,
		[ "some"  => sub { $val3 = 'some'; } ],
		[ "other" => sub { $val3 = 'other'; } ],
		[ eof     => sub { $val3 = 'eof'; } ],
		[ timeout => sub { $val3 = 'timeout'; } ],
	);
	is $val3, 'eof';
};


subtest raw_reversing => sub {
	diag "Testing raw reversing...";
	plan tests => 12;

	my @Strings = (
		"The quick brown fox jumped over the lazy dog.",
		"Ein Neger mit Gazelle zagt im Regen nie",
		"Was ich brauche ist ein Lagertonnennotregal",
	);

	my $exp = Expect->new;

	#    my $exp = Expect->new("$Perl -MIO::File -ne 'BEGIN {\$|=1; \$in = IO::File->new( \">reverse.in\" ) or die; \$in->autoflush(1); \$out = IO::File->new( \">reverse.out\" ) or die; \$out->autoflush(1); } chomp; print \$in \"\$_\\n\"; \$_ = scalar reverse; print \"\$_\\n\"; print \$out \"\$_\\n\"; '");

	diag "isatty(\$exp): " . (POSIX::isatty($exp) ? "YES" : "NO");

	$exp->raw_pty(1);
	$exp->spawn("$Perl -ne 'chomp; sleep 0; print scalar reverse, \"\\n\"'")
		or die "Cannot spawn $Perl: $!\n";
	my $called = 0;
	$exp->log_file( sub { $called++; } );
	foreach my $s (@Strings) {
		my $rev = scalar reverse $s;
		$exp->send("$s\n");
		$exp->expect(
			10,
			[ quotemeta($rev) => sub { ok(1); } ],
			[ timeout => sub { ok(0); die "Timeout"; } ],
			[ eof     => sub { ok(0); die "EOF"; } ],
		);
	}
	ok( $called >= @Strings );
	$exp->log_file(undef);

	# now with send_slow
	$called = 0;
	$exp->log_file( sub { $called++; } );
	my $delay = 0.1;
	foreach my $s (@Strings) {
		my $rev = scalar reverse $s;
		my $now = time;
		$exp->send_slow( $delay, "$s\n" );
		$exp->expect(
			10,
			[ quotemeta($rev) => sub { ok(1); } ],
			[ timeout => sub { ok(0); die "Timeout"; } ],
			[ eof     => sub { ok(0); die "EOF"; } ],
		);
		my $dur = time - $now;
		ok( $dur > length($s) * $delay );
	}
	ok( $called >= @Strings );
	$exp->log_file(undef);


	diag <<_EOT_;

------------------------------------------------------------------------------
>  The following tests check system-dependend behaviour, so even if some fail,
>  Expect might still be perfectly usable for you!
------------------------------------------------------------------------------
_EOT_

	# we check if the raw pty can handle large chunks of text at once

	my $randstring =
		'fakjdf ijj845jtirg8e 4jy8 gfuoyhjgt8h gues9845th guoaeh gt98hae 45t8u ha8rhg ue4ht 8eh tgo8he4 t8 gfj aoingf9a8hgf uain dgkjadshftuehgfusand987vgh afugh 8h 98H 978H 7HG zG 86G (&g (O/g &(GF(/EG F78G F87SG F(/G F(/a sldjkf hajksdhf jkahsd fjkh asdHJKGDSGFKLZSTRJKSGOSJDFKGHSHGDFJGDSFJKHGSDFHJGSDKFJGSDGFSHJDGFljkhf lakjsdh fkjahs djfk hasjkdh fjklahs dfkjhasdjkf hajksdh fkjah sdjfk hasjkdh fkjashd fjkha sdjkfhehurthuerhtuwe htui eruth ZI AHD BIZA Di7GH )/g98 9 97 86tr(& TA&(t 6t &T 75r 5$R%/4r76 5&/% R79 5 )/&';
	my $maxlen;
	$exp->log_stdout(0);
	$exp->log_file("$tempdir/test.log");
	my $exitloop;
	$SIG{ALRM} = sub { die "TIMEOUT on send" };

	foreach my $len ( 1 .. length($randstring) ) {
		#print "$len\r";
		my $s = substr( $randstring, 0, $len );
		my $rev = scalar reverse $s;
		eval {
			alarm(10);
			$exp->send("$s\n");
			alarm(0);
		};
		if ($@) {
			ok( $maxlen > 80 );
			print "Warning: your raw pty blocks when sending more than $maxlen bytes!\n";
			$exitloop = 1;
			last;
		}
		$exp->expect(
			10,
			[ quotemeta($rev) => sub { $maxlen = $len; } ],
			[   timeout => sub {
					ok( $maxlen > 160 );
					print "Warning: your raw pty can only handle $maxlen bytes at a time!\n";
					$exitloop = 1;
				}
			],
			[ eof => sub { ok(0); die "EOF"; } ],
		);
		last if $exitloop;
	}
	$exp->log_file(undef);
	diag "Good, your raw pty can handle at least " . length($randstring) . " bytes at a time." if not $exitloop;
	cmp_ok $maxlen, '>', 160;
};

# Now test for the max. line length. Some systems are limited to ~255
# chars per line, after which they start loosing characters.  As Cygwin
# then hangs and cannot be freed via alarm, we only test up to 160 characters
# to avoid that.

subtest max_line_length => sub {
	plan tests => 1;

	my $exp = Expect->new(qq{$Perl -ne 'chomp; sleep 0; print scalar reverse, "\\n"'})
		or die "Cannot spawn $Perl: $!\n";

	$exp->log_stdout(0);
	my $randstring =
		'Fakjdf ijj845jtirg8 gfuoyhjgt8h gues9845th guoaeh gt9vgh afugh 8h 98H 97BH 7HG zG 86G (&g (O/g &(GF(/EG F78G F87SG F(/G F(/a slkf ksdheq@f jkahsd fjkh%&/"§ä#üßw';
	diag 'Length: ' . length($randstring);
	my $status = '';
	my $maxlen = 0;
	my $exitloop;
	foreach my $len ( 1 .. length($randstring) ) {
		#print "$len\r";
		my $s = substr( $randstring, 0, $len );
		my $rev = scalar reverse $s;
		eval {
			alarm(10);
			$exp->send("$s\n");
			alarm(0);
		};
		if ($@) {
			#ok( $maxlen > 80 );
			diag "Warning: your default pty blocks when sending more than $maxlen bytes per line!";
			$status = 'block';
			$exitloop = 1;
			last;
		}
		$exp->expect(
			10,
			[ quotemeta($rev) => sub { $maxlen = $len; $status = 'match' } ],
			[ timeout => sub {
					diag "Warning: your default pty can only handle $maxlen bytes at a time!\n";
					$status = 'limit';
					$exitloop = 1;
				}
			],
			[ eof => sub { $status = 'eof'; die "EOF"; } ],
		);
		last if $exitloop;
	}
	diag "Good, your default pty can handle lines of at least " . length($randstring) . " bytes at a time."
		if not $exitloop;
	diag "Status: $status";
	cmp_ok $maxlen, '>', 100;
};

subtest controlling_termnal => sub {
	diag "Testing controlling terminal...";
	plan tests => 1;

	my $exp =
		Expect->new( $Perl
			. q{ -MIO::Handle -e 'open(TTY, "+>/dev/tty") or die "no controlling terminal"; autoflush TTY 1; print TTY "Expect_test_prompt: "; $s = <TTY>; chomp $s; print "uc: \U$s\n"; close TTY; exit 0;'}
		);

	my $pwd = "pAsswOrd";
	$exp->log_file("$tempdir/test_dev_tty.log");
	my $val = '';
	$exp->expect(
		10,
		[   qr/Expect_test_prompt:/,
			sub {
				my $self = shift;
				$self->send("$pwd\n");
				$exp->log_file(undef);
				exp_continue;
			}
		],
		[   qr/(?m:^uc:\s*(\w+))/,
			sub {
				my $self = shift;
				my ($s) = $self->matchlist;
				chomp $s;
				$val = $s;
			}
		],
		[ eof     => sub { $val = 'eof'; die "EOF"; } ],
		[ timeout => sub { $val = 'timeout'; die "Timeout"; } ],
	);
	is $val, uc($pwd);
};


subtest exit_status => sub {
	diag "Checking if exit status is returned correctly...";
	plan tests => 3;

	my $exp = Expect->new( $Perl . q{ -e 'print "Expect_test_pid: $$\n"; sleep 2; exit(42);'} );
	my $val = '';
	$exp->expect(
		10,
		[ qr/Expect_test_pid:/, sub { my $self = shift; $val = 'test_pid'; } ],
		[ eof     => sub { $val = "eof"; } ],
		[ timeout => sub { $val = "timeout"; } ],
	);
	is $val, 'test_pid';
	my $status = $exp->soft_close();
	diag sprintf "soft_close: 0x%04X\n", $status;
	is $exp->exitstatus(), $status;
	is( ( ( $status >> 8 ) & 0x7F ), 42);
};


subtest signal => sub {
	diag "Checking if signal exit status is returned correctly...";
	plan tests => 3;

	my $exp = Expect->new( $Perl . q{ -e 'print "Expect_test_pid: $$\n"; sleep 2; kill 15, $$;'} );
	my $val = '';
	$exp->expect(
		10,
		[ qr/Expect_test_pid:/, sub { my $self = shift; $val = 'test_pid'; } ],
		[ eof     => sub { $val = "eof"; } ],
		[ timeout => sub { $val = "timeout"; } ],
	);
	is $val, 'test_pid';
	my $status = $exp->soft_close();
	diag sprintf "soft_close: 0x%04X", $status;
	ok( $exp->exitstatus() == $status );
	my ( $hi, $lo ) = ( ( $status >> 8 ) & 0x7F, $status & 0x7F );

	ok( $hi == 15 or $lo == 15 );
};

diag <<__EOT__;

Checking if EOF on pty slave is correctly reported to master...
(this fails on about 50% of the supported systems, so don't panic!
 Expect will work anyway!)

__EOT__

subtest eof_on_pty => sub {
	plan tests => 1;

	my $exp = Expect->new( $Perl . q{ -e 'close STDIN; close STDOUT; close STDERR; sleep 3;'} );
	my $res;
	$exp->expect(
		2,
		[ eof     => sub { $res = 'eof' } ],
		[ timeout => sub { $res = 'timeout' } ],
	);
	is $res, 'timeout', "Sorry, you may not notice if the spawned process closes the pty.";
	$exp->hard_close();
};


use Test::Builder;
my $Test = Test::Builder->new;
diag <<__EOT__ if ( not $Test->is_passing );
Please scroll back and check which test(s) failed and what comments
were given.  Expect probably is still completely usable!!
__EOT__

exit(0);

