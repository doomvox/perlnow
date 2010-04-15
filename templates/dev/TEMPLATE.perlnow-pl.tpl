#!/usr/bin/perl
# (>>>FILE<<<)                   (>>>AUTHOR<<<)
# (>>>PNFS<<<)                   (>>>DATE<<<)

=head1 NAME

(>>>FILE<<<) - (( TODO insert brief description ))

=head1 SYNOPSIS

  (>>>FILE<<<) -[options] [arguments]

  Options:
     -d          debug messages on
     --debug     same
     -h          help (show usage)
     -v          show version
     --version   show version

=cut

use warnings;
use strict;
$|=1;
use Carp;
use Data::Dumper;

use File::Path     qw( mkpath );
use File::Basename qw( fileparse basename dirname );
use File::Copy     qw( copy move );
use Fatal          qw( open close mkpath copy move );
use Cwd            qw( cwd abs_path );
use Env            qw( HOME );
use Config::Std;
use Getopt::Long   qw( :config no_ignore_case bundling );

our $VERSION = 0.01;
my  $prog    = basename($0);

my $DEBUG   = 1;                 # TODO set default to 0 when in production
GetOptions ("d|debug"    => \$DEBUG,
            "v|version"  => sub{ say_version(); },
            "h|?|help"   => sub{ say_usage();   },
           ) or say_usage();
#           "length=i" => \$length,        # numeric
#           "file=s"   => \$file,          # string

(>>>POINT<<<)




### end main, into the subs

sub say_usage {
  my $usage=<<"USEME";
   $prog <-options> <arguments>
     TODO fill-in usage statement
USEME
  print "$usage\n";
  exit;
}

sub say_version {
  print "Running $prog version: $VERSION\n";
  exit 1;
}


__END__


=head1 DESCRIPTION

B<(>>>FILE<<<)> is a script which

(( TODO  insert explaination
   This is stub documentation created by template.el.  ))

=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

=head1 BUGS

None reported... yet.

=cut
