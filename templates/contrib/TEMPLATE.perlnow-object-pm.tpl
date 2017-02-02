package (>>>PERL_MODULE_NAME<<<);

use (>>>MINIMUM_PERL_VERSION<<<);
use strict;
use warnings;
use base qw(   );

our $VERSION = "0.4";

use Carp;
use Class::Std;
use Log::Log4perl qw( get_logger );

{
    sub _get_log {
        return get_logger("(>>>POINT<<<)");
    }

    ###########
    # WARNING #
    # Do not change the whitespace in the :ATTR declarations.  Class::Std
    # is broken; all leading whitespace (except for newlines) causes
    # code to fail.
    # my %name_of :ATTR( init_arg => "name");

    ########################
    # Operator overloading #
    sub as_number  : NUMERIFY  {
        my ($self, $ident) = @_;
        my $log = _get_log();
        my $class_name = ref $self;

        $log->logcroak("Can't coerce an object of class $class_name",
                       " to a number");
    }

    sub as_string  : STRINGIFY {
        my ($self, $ident) = @_;
        my $log = _get_log();
        my $class_name = ref $self;

        $log->logcroak("Can't coerce an object of class $class_name",
                       " to a string");
    }

    sub as_boolean : BOOLIFY   {
        my ($self, $ident) = @_;
        my $log = _get_log();
        my $class_name = ref $self;

        $log->logcroak("Can't coerce an object of class $class_name",
                       " to a boolean");
    }
}

1;
__END__
# Below is stub documentation for your module. You'd better edit it!

=head1 NAME

(>>>PERL_MODULE_NAME<<<) - Perl extension for blah blah blah

=head1 SYNOPSIS

   use (>>>PERL_MODULE_NAME<<<);
   (>>>7<<<)blah blah blah

=head1 DESCRIPTION

(>>>6<<<)Stub documentation for (>>>PERL_MODULE_NAME<<<),
created by perlnow.el using template.el.

It looks like the author of the extension was negligent
enough to leave the stub unedited.

Blah blah blah.

=head2 EXPORT

None by default.

=head1 SEE ALSO

=head1 AUTHOR

(>>>USER_NAME<<<), E<lt>(>>>EMAIL_DOT_EMACS<<<)E<gt>

=head1 COPYRIGHT AND LICENSE

Copyright (C) (>>>YEAR<<<) by (>>>USER_NAME<<<)

(>>>LICENSE<<<)
