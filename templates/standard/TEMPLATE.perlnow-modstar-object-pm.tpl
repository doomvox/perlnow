# An OOP template for module-starter
package (>>>PERL_MODULE_NAME<<<);
use Moo;
use MooX::Types::MooseLike::Base qw(:all);

=head1 NAME

(>>>PERL_MODULE_NAME<<<) - The great new (>>>PERL_MODULE_NAME<<<)! TODO revise this

=head1 VERSION

Version 0.01

=cut

# TODO revise these before shipping
our $VERSION = '0.01';
my $DEBUG = 1;

=head1 SYNOPSIS

   use (>>>PERL_MODULE_NAME<<<);
   my $obj = (>>>PERL_MODULE_NAME<<<)->new({ ...  });

   # TODO expand on this

=head1 DESCRIPTION

(>>>PERL_MODULE_NAME<<<) is a module that ...

TODO expand this stub documentation, which was created by perlnow.el.

=head1 METHODS

=over

=cut

use (>>>MINIMUM_PERL_VERSION<<<);
use Carp;
use Data::Dumper;

# needed for accessor generation
our $AUTOLOAD;

=item new

Creates a new (>>>PERL_MODULE_NAME<<<) object.

Takes a hashref as an argument, with named fields identical
to the names of the object attributes. These attributes are:

=over

=item <TODO fill-in attributes here... most likely, sort in order of utility>

=back

=cut

# Example attribute:
# has is_loop => ( is => 'rw', isa => Int, default => 0 );
# Tempted to use Mouse over Moo so I can do my usual "isa => 'Int'"

# $DB::single = 1;

### Fill in additional methods here
### hint: perlnow-insert-method

(>>>POINT<<<)


=back

=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>,
(>>>DATE<<<)

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

=head1 BUGS

Please report any bugs or feature requests to C<bug-emacs-run at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Emacs-Run>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.

=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Emacs::Run

You can also look for information at:

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Emacs-Run>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Emacs-Run>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Emacs-Run>

=item * Search CPAN

L<http://search.cpan.org/dist/Emacs-Run/>

=back

=head1 ACKNOWLEDGEMENTS


=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)

This program is free software; you can redistribute it and/or modify it
under the terms of either: the GNU General Public License as published
by the Free Software Foundation; or the Artistic License.

See http://dev.perl.org/licenses/ for more information.

=cut

1;
