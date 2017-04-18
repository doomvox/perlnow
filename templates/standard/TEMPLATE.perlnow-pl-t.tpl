# Perl test file, which can be run like so:
#   `perl (>>>FILE<<<)'
#        (>>>AUTHOR<<<)     (>>>VC_DATE<<<)

use warnings;
use strict;
$|=1;
my $DEBUG = 1;
use Data::Dumper;
use File::Path      qw( mkpath );
use File::Basename  qw( fileparse basename dirname );
use File::Copy      qw( copy move );
use Fatal           qw( open close mkpath copy move );
use Cwd             qw( cwd abs_path );
use Env             qw( HOME );
use List::MoreUtils qw( any );

use Test::More;
BEGIN { plan tests => 1 };  # TODO # change to 'tests => last_test_to_print';

ok(1, "Traditional: If we made it this far, we're ok.");

my $script_name = '(>>>PERL_SCRIPT_NAME<<<)';
my $script_basename = basename( $script_name );

{
  my $test_name = "Testing script $script_basename";

# TODO Enter the expected output from the script
my $expected=<<"EXPECTED";
(>>>POINT<<<)
EXPECTED

  # TODO any arguments to add after the script name?
  my $result = qx{ $script_name };
  is( $result, $expected, "$test_name" )
}

