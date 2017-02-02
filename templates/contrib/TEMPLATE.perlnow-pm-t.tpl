# Run this like so: `perl (>>>FILE<<<)'

use warnings;
use strict;
$|=1;

use Test::More qw( no_plan );

(>>>9<<<)

use_ok("(>>>PERL_MODULE_NAME<<<)");

ok((>>>POINT<<<), );
# INCOMPLETE

__END__

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)
