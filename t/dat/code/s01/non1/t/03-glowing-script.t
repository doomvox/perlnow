# Perl test file, which can be run like so:
#   `perl 03-glowing-script.t'
#        doom@kzsu.stanford.edu     2017/06/06 20:50:13

use 5.008;
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

ok(1, "Traditional: If we made it this far, we're ok.");

my $script_name = '/home/doom/End/Cave/Perlnow/lib/perlnow/t/dat/code/s65/non1/bin/glowing.pl';
my $script_basename = basename( $script_name );

{
  my $test_name = "Testing script $script_basename";

# TODO Enter the expected output from the script
my $expected=<<"EXPECTED";

EXPECTED

  # TODO any arguments to add after the script name?
  my $result = qx{ $script_name };
  is( $result, $expected, "$test_name" )
}

done_testing();
