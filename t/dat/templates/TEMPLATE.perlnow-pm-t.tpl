# Perl test file, can be run like so:
#   `perl (>>>FILE<<<)'
#         (>>>AUTHOR<<<)     (>>>VC_DATE<<<)

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
BEGIN { plan tests => 3 }; # TODO revise test count

use_ok( '(>>>PERL_MODULE_NAME<<<)' );

ok(1, "Traditional: If we made it this far, we're ok.");

# Insert your test code below.  Consult perldoc Test::More for help.

{  my $subname = "(>>>PERL_SUB_NAME<<<)";
   my $test_name = "Testing $subname";

   # (>>>PERL_MODULE_NAME<<<)
   # (>>>PERL_SUB_NAME<<<)

    (>>>POINT<<<)
 }
