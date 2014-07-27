use ExtUtils::MakeMaker;

WriteMakefile(
	NAME         => 'Expect',
	VERSION_FROM => 'lib/Expect.pm',
	PREREQ_PM    => {
		'IO::Tty' => 1.03,
		'IO::Pty' => 1.03,
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

