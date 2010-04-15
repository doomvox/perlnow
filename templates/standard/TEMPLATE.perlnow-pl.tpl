#!/usr/bin/perl
# (>>>FILE<<<)                   (>>>AUTHOR<<<)
# (>>>PNFS<<<)                   (>>>DATE<<<)

use warnings;
use strict;
$|=1;
use Data::Dumper;

use File::Path qw(mkpath);
use File::Basename;
use Env qw(HOME);

use Getopt::Std;
my %opts;
getopts('d', \%opts);
my $DEBUG = $opts{d} || 1;


(>>>POINT<<<)



__END__

=head1 NAME

(>>>FILE<<<)

=head1 SYNOPSIS

=head1 DESCRIPTION

Stub documentation for (>>>FILE<<<),
created by template.el.

It looks like the author of this script was negligent
enough to leave the stub unedited.


=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

This program is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version 5.8.2 or,
at your option, any later version of Perl 5 you may have available.

=head1 BUGS

None reported... yet.

=cut
