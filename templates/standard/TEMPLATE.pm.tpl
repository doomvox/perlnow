package (>>>module-name<<<);
#                                (>>>AUTHOR<<<)
#                                (>>>DATE<<<)


=head1 NAME

(>>>module-name<<<) - Perl extension for blah blah blah

=head1 SYNOPSIS

   use (>>>module-name<<<);
   (>>>7<<<)blah blah blah

=head1 DESCRIPTION

(>>>6<<<)Stub documentation for (>>>module-name<<<),
created by template.el.

It looks like the author of the extension was negligent
enough to leave the stub unedited.

Blah blah blah.

=head2 EXPORT

None by default.  Optionally:

=over

=cut

use (>>>MINIMUM_PERL_VERSION<<<);
use strict;
use warnings;
use Carp;
use Data::Dumper;

require Exporter;

our (@ISA, @EXPORT_OK, %EXPORT_TAGS, @EXPORT);
BEGIN {
 require Exporter;
 @ISA = qw(Exporter);
 %EXPORT_TAGS = ( 'all' => [
 # TODO Add names of items to export here.
 qw(
     (>>>9<<<)
    ) ] );
  # The above allows declaration	use (>>>PERL_MODULE_NAME<<<) ':all';

  @EXPORT_OK = ( @{ $EXPORT_TAGS{'all'} } );
  @EXPORT = qw(  ); # items to export into callers namespace by default (avoid this!)
}

our $VERSION = '0.01';

# Preloaded methods go here.

(>>>POINT<<<)

1;
__END__

=back

=head1 SEE ALSO

TODO Mention other useful documentation:

  o  related modules:  L<Module::Name>
  o  operating system documentation (such as man pages in UNIX)
  o  any relevant external documentation such as RFCs or standards
  o  discussion forum set up for your module (if you have it)
  o  web site set up for your module (if you have it)

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

This library is free software; you can redistribute it and/or modify
it under the same terms as Perl itself, either Perl version (>>>MINIMUM_PERL_VERSION<<<) or,
at your option, any later version of Perl 5 you may have available.

=cut

>>>TEMPLATE-DEFINITION-SECTION<<<
("module-name" "Module Package Name: ")
