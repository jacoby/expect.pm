use ExtUtils::MakeMaker;

WriteMakefile(
	NAME         => 'Expect',
	VERSION_FROM => 'lib/Expect.pm',
	PREREQ_PM    => {
		'IO::Tty' => 1.11,
		'IO::Pty' => 1.11,
	},
	BUILD_REQUIRES => {
		'Test::More'   => '1.00',
	},
	ABSTRACT_FROM => 'lib/Expect.pm',
	clean        => { 'FILES' => '*.log' },
	dist         => { COMPRESS => 'gzip -9f', SUFFIX => 'gz' },
	($ExtUtils::MakeMaker::VERSION >= 6.3002 ? ('LICENSE'=> 'perl') : ()),
	(eval { ExtUtils::MakeMaker->VERSION(6.46) } ? (META_MERGE => {
		'meta-spec' => { version => 2 },
		resources => {
			repository => {
				type => 'git',
				url  => 'http://github.com/szabgab/expect.pm.git',
				web  => 'http://github.com/szabgab/expect.pm',
			}
		}})
		: ()
	),
);

