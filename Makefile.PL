use ExtUtils::MakeMaker;

WriteMakefile(
	NAME         => 'Expect',
	VERSION_FROM => 'lib/Expect.pm',
	PREREQ_PM    => {
		'IO::Tty' => 1.11,
		'IO::Pty' => 1.11,

		# standard modules:
		'POSIX'      => 0,
		'Fcntl'      => 0,
		'Carp'       => 0,
		'IO::Handle' => 0,
		'Exporter'   => 0,
		'Errno'      => 0,
	},
	BUILD_REQUIRES => {
		'Test::More'      => '1.00',
		'Test::Exception' => '0.32';

		# standard modules:
		'File::Temp' => 0,
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

