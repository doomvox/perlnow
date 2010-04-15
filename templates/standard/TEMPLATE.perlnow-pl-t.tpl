# Test file created outside of h2xs framework.
# Run this like so: `perl (>>>FILE<<<)'
#   (>>>AUTHOR<<<)     (>>>VC_DATE<<<)

#########################

# change 'tests => 1' to 'tests => last_test_to_print';

use warnings;
use strict;
$|=1;
my $DEBUG = 1;
use Data::Dumper;

use Test::More;
BEGIN { plan tests => 1 };

ok(1); # If we made it this far, we're ok.

is( `(>>>PERL_SCRIPT_NAME<<<)` , (>>>POINT<<<), "Test name: (>>>2<<<)")
# Enter the expected output from the script as the second argument
# Possibly add arguments after the script name given in the first agrument
# ((would it be better to fill in a runstring?))

#########################

# Insert your test code below, the Test::More module is used here so read
# its man page ( perldoc Test::More ) for help writing this test script.


