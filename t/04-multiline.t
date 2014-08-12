use strict;
use warnings;

use Test::More tests => 19;
use Expect;

my $e = Expect->new;
$e->raw_pty(1);
$e->log_stdout(0);
$e->spawn($^X . q{ -ne 'chomp; print "My\nHello\n"; print scalar reverse; print "\nWorld\nAnd\nMore\n"' });

{
	my $reply;
	$e->send("abc\n");
	$e->expect(1, ['^cba$' => sub { $reply = $e->match } ]);
	is $reply, 'cba', 'reply';
	is $e->before, "My\nHello\n";
	is $e->after, "\nWorld\nAnd\nMore\n";
}

{
	$e->send("def\n");
	$e->expect(1, ['^fed$']);
	is $e->match, 'fed', 'match';
	$e->clear_accum;
}

{
	$e->send("dnAX\n");
	$e->expect(1, '-re', '^X.*d$');
	is $e->match, 'XAnd', 'match';
	$e->clear_accum;
}

{
	$e->send("eroM\n");
	$e->expect(1, '-re', '^M(..)e$');
	is $e->match, 'More', 'match';
	$e->clear_accum;
}


{
	$e->send("dnAX\n");
	$e->expect(1, '-re', '^X(?s:.*)d$');
	is $e->match, "XAnd\nWorld\nAnd", 'match';
	$e->clear_accum;
}


{
	$e->send("ghi\n");
	$e->expect(1, '-re', '^ihg$');
	is $e->match, 'ihg', 'match';
	$e->clear_accum;
}


TODO: {
	local $TODO = 'Multiline_Maching does not work when qr// is passed. (Should it work?)';
	my $reply;
	$e->send("zorg\n");
	$e->expect(1, [qr/^groz$/ => sub { $reply = $e->match } ]);
	is $reply, 'groz';
}

{
	local $Expect::Multiline_Matching = 0;
	my $reply;
	$e->send("abc\n");
	$e->expect(1, ['^cba$' => sub { $reply = $e->match } ]);
	is $reply, undef, 'reply';
}

{
	local $Expect::Multiline_Matching = 0;
	$e->clear_accum;
	$e->send("def\n");
	$e->expect(1, ['^fed$']);
	#diag $e->before;
	TODO: {
		local $TODO = 'Why does the match return ihg in this example?';
		# it seems ->match will return the string of the last successful match
		# even if there were unsuccessful ->expect calls in the middle.
		# This sounds like a bug.
		is $e->match, undef, 'match';
	}
	is $e->match, 'ihg', 'maybe it should return ihg';
}

{
	local $Expect::Multiline_Matching = 0;
	$e->send("mno\n");
	$e->expect(1, '-re', '^onm$');
	TODO: {
		local $TODO = 'Why does the match return ihg in this example?';
		is $e->match, undef, 'match';
	}
	is $e->match, 'ihg', 'maybe it should return ihg';
}

{
	local $Expect::Multiline_Matching = 0;
	$e->send("dnAX\n");
	$e->expect(1, '-re', '^X.*d$');
	is $e->match, 'ihg', 'match';  # TODO: IMHO this should be undef as well.
	$e->clear_accum;
}

{
	local $Expect::Multiline_Matching = 0;
	$e->send("dnAX\n");
	$e->expect(1, '-re', '^X(?s:.*)d$');
	is $e->match, "ihg", 'match'; # TODO ??
	$e->clear_accum;
}


{
	local $Expect::Multiline_Matching = 0;
	$e->send("dnAX\n");
	$e->expect(1, '-re', 'X.*d');   # no ^ and $
	is $e->match, 'XAnd', 'match';
	$e->clear_accum;
}

{
	local $Expect::Multiline_Matching = 0;
	$e->send("dnAX\n");
	$e->expect(1, '-re', 'X(?s:.*)d'); # no ^ and $
	is $e->match, "XAnd\nWorld\nAnd", 'match';
	$e->clear_accum;
}


{
	#diag 'localized $Expect::Multiline_Matching = 0;  has no effect after the block:';
	$e->send("abc\n");
	$e->expect(1, ['^cba$']);
	is $e->match, 'cba', 'match';
}

