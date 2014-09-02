use strict;
use warnings;

use Test::More;

plan tests => 16;

use Expect;
my $e = Expect->new;

$e->spawn($^X, "examples/calc.pl") or die;

$e->log_stdout(0);
#$e->raw_pty(1);

is $e->match, undef;
is $e->before, undef;
is $e->after, undef;
is $e->get_accum, '';


$e->send("19+23\n");
$e->expect(1, "19+23");
is $e->match, '19+23', 'match';
is $e->before, '', 'before';
like $e->after, qr/^\s*$/, 'after';

$e->clear_accum;
$e->expect(1, '-re' => qr/'\d+'/);
is $e->match, q{'42'};
is $e->before, q{Input: '19+23' = };
like $e->after,  qr/^ :Output\s*$/;

$e->expect(1, 'abc');
is $e->match, undef;
like $e->before,  qr/^ :Output\s*$/;
is $e->after,  undef;

$e->clear_accum;
$e->expect(1, 'def');
is $e->match, undef;
is $e->before, '';    #??
is $e->after,  undef;


$e->close;

# These tests faled on midnightbsd, and on almost all cases of gnukfreebsd
# http://www.cpantesters.org/cpan/report/fb542c9a-3253-11e4-a396-829772410e08
# one was successful though.
# On netbased failed on all except this one:
# http://www.cpantesters.org/cpan/report/fd895392-2bbd-11e4-b698-db7a2867dcfa
