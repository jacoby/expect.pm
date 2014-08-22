use strict;
use warnings;

use Test::More;

plan tests => 12;

use Expect;
my $e = Expect->new;

$e->spawn($^X, "examples/calc.pl") or die;

$e->log_stdout(0);
$e->raw_pty(1);

is $e->match, undef;
is $e->before, undef;
is $e->after, undef;


$e->send("19+23\n");
$e->expect(1, "19+23");
is $e->match, '19+23';
is $e->before, '';
like $e->after, qr/^\s*$/;

$e->clear_accum;
$e->expect(1, '-re' => qr/'\d+'/);
is $e->match, q{'42'};
is $e->before, q{Input: '19+23' = };
like $e->after,  qr/^ :Output\s*$/;

$e->expect(1, 'abc');
is $e->match, q{'42'};
like $e->before,  qr/^ :Output\s*$/;
like $e->after,  qr/^ :Output\s*$/;

$e->close;

