# Perl test file, can be run like so:
#   `perl 02-Marble-CatsEye-fire.t'
#         doom@kzsu.stanford.edu     2017/07/19 12:39:13

use 5.008;
use warnings;
use strict;
$|=1;
my $DEBUG = 1;              # TODO set to 0 before ship
use Data::Dumper;
use File::Path      qw( mkpath );
use File::Basename  qw( fileparse basename dirname );
use File::Copy      qw( copy move );
use Fatal           qw( open close mkpath copy move );
use Cwd             qw( cwd abs_path );
use Env             qw( HOME );
use List::MoreUtils qw( any );

use Test::More;

BEGIN {
  use_ok( 'Marble::CatsEye' ,  qw(fire) )
}

ok(1, "Traditional: If we made it this far, we're ok.");

# $DB::single = 1;
# Insert your test code below.  Consult perldoc Test::More for help.

{  my $subname = "fire";
   my $test_name = "Testing $subname";

   # use Marble::CatsEye;
   # fire();
   my $expected = "clink";
   my $ret = fire();
   is( $ret, $expected, $test_name );

 }

done_testing();
