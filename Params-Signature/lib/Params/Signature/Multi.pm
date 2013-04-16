package Params::Signature::Multi;
use 5.006;
use strict;

use Carp;
use Params::Signature;
use Data::Dumper;

my $debug = 0;
my $class_default = undef;
*class_default = sub { ($class_default) ? $class_default : ($class_default = new Params::Signature::Multi(on_fail => \&confess)) };

sub new
{
    my $class = shift;
    my $self = {};
    my %params = @_;

    bless $self, $class;

    $self->{validator} = new Params::Signature(@_, on_fail => sub { $self->fail(@_) });

    $self->{on_fail} = $params{on_fail};

    return $self;
}

# set custom Params::Signature object as validator
# NOTE: this is NOT a class method
sub validator
{
    # $self not set to $class_default so that people don't mess with "system defaults"
    my $self = shift;
    if (scalar @_)
    {
        $self->{validator} = shift;
        $self->{on_fail} = $self->{validator}->{on_fail};
        $self->{validator}->{on_fail} = sub {$self->fail(@_)};
    }
    return $self->{validator};
}

# Parameters Needed:
# subroutine parameters (values passed to caller)
# list of signatures
#   signature
#   sub reference
# $multi->dispatch(\@_, 
#                   [
#                       { signature => ["Int one", "Str two"], call => &sub_one },
#                       { signature => ["Str one", "Str two", "Str three"], call => &sub_two },
#                   ],
#                   );
# $multi->dispatch(params => \@_,
#                   signatures =>
#                       [
#                           { signature => ["Int one", "Str two"], call => &sub_one },
#                           { signature => ["Str one", "Str two", "Str three"], call => &sub_two },
#                       ],
#                   on_fail => \&call_me
#                   );
#                 
#
# store "prototypes" in a local (package-level) hash
# ... now both the actual sub and dispatch can easily share the same signature
# $prototype{'sub_one_a' => ["Int one", "Str two"]};
# $prototype{'sub_one_b' => ["Str one", "Str two", "Str three"]};
# $prototype{'method_one_a' => ["Object self", "Str one", "Str two", "Str three"]};
# $prototype{'method_two_b' => ["Object self", "Str one", "Str two", "Str three"]};
# $multi->dispatch(\@_, 
#                   [
#                       { signature => $prototype{"sub_one_a"}, call => &sub_one_a },
#                       { signature => $prototype{"sub_two_b"}, call => &sub_two_b },
#                   ],
#                   );
# $multi->dispatch(\@_, 
#                   [
#                       { signature => $prototype{"method_one_a"}, call => &method_one_a },
#                       { signature => $prototype{"method_one_b"}, call => &method_one_b },
#                   ],
#                   );
sub dispatch
{
    my $self = (ref($_[0])) ? shift @_ : (shift @_ and class_default());
    my %params = @_;
    my $idx;
    my $id;
    my $on_fail;

    if (scalar @_ == 2)
    {
        $params{params} = shift;
        $params{signatures} = shift;
    }
    elsif (scalar @_ == 3)
    {
        $params{params} = shift;
        $params{signatures} = shift;
        $params{param_style} = shift;
    }
    else
    {
        %params = @_;
    }

    $on_fail = (defined($params{on_fail})) ? $params{on_fail} : $self->{on_fail};

    ($idx, $id) = $self->resolve(%params);
    if ($self->{ok})
    {
        print_debug("Resolved to: $idx, $id ...");
        if (defined($params{signatures}->[$idx]{call}))
        {
            return $params{signatures}->[$idx]{call}->(@{$params{params}});
        }
        else
        {
            $on_fail->("No subroutine to call");
        }
    }
    return;
}

# in scalar context return id (if set) or index (if no id set)
# in list context, return index and id (undef if not set) 
# $multi->resolve(\@_, 
#                   [
#                       { signature => ["Int one", "Str two"], id => 'one' },
#                       { signature => ["Str one", "Str two", "Str three"], id => 'two' },
#                   ]
#                   );
# $multi->resolve(\@_, 
#                   [
#                       #implicit id of "0"
#                       { signature => ["Int one", "Str two"] },
#                       #implicit id of "1"
#                       { signature => ["Str one", "Str two", "Str three"] },
#                   ]
#                   );
# $multi->resolve(params => \@_, 
#                 param_style => "positional|named|mixed",
#                 signatures => [
#                       #implicit id of "0"
#                       { signature => ["Int one", "Str two"] },
#                       #implicit id of "1"
#                       { signature => ["Str one", "Str two", "Str three"] },
#                   ],
#                 on_fail => \&call_me
#                   );
sub resolve
{
    my $self = (ref($_[0])) ? shift @_ : (shift @_ and class_default());
    my %params;
    my %validate_params;
    my $i;
    my $param_style;
    my $param;
    my @param_values;
    my $passed = -1;
    my $id = undef;
    my $on_fail;

    print_debug("Params::Signature::Multi::resolve:", Dumper(\@_));

    if (scalar @_ == 2)
    {
        $params{params} = shift;
        $params{signatures} = shift;
    }
    else
    {
        %params = @_;
    }

    $on_fail = (defined($params{on_fail})) ? $params{on_fail} : $self->{on_fail};

    for($i =0; $i < scalar @{$params{signatures}}; $i++)
    {
        $params{check_only} = 1;
        $self->{ok} = 1;
        # might be faster to 'delete' id and then add back after validate()
        %validate_params = map { ($_ ne 'id' && $_ ne 'call')  ? ($_, $params{signatures}->[$i]{$_}) : () } keys %{$params{signatures}->[$i]};
        $validate_params{params} = $params{params};
        print_debug("Validate params:", Dumper(\%validate_params));
        $param_style = $self->{validator}->validate(%validate_params);
        print_debug("Validate result: (ok=1) " . $self->{ok}); 
        if ($self->{ok})
        {
            $passed = $i;
            $id = $params{signatures}[$i]->{id};
            last;
        }
    }

    if (($passed < 0) && ($on_fail))
    {
        print_debug("Call on_fail with: Failed to resolve subroutine using parameters and signatures");
        $on_fail->("Failed to resolve subroutine using parameters and signatures");
    }

    # list context: index and id are returned
    # scalar context: id, if set, else index
    return (wantarray ? ($passed, $id) : ($id || $passed)); 
}


sub fail
{
    my $self = shift;
    my $msg = shift;
    print_debug("Dang it! $msg");
    $self->{ok} = 0;
    $self->{msg} = $msg;
}


sub print_debug
{
    if ($debug)
    {
        print STDERR join("\n", @_, "\n");
    }
}

1; 
__END__

=head1 NAME

Params::Signature::Multi - support for subroutine selection and dispatch based on subroutine signature and parameters

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

    use Params::Signature::Multi;

    my $prototypes = {
        sub_one_a => ["Int one", "Int two"],
        sub_one_b => ["Int one", "Str two"],
        sub_one_fallback => ["..."],
        method_two_a => ["Object self", "Int one", "Int two"],
        method_two_b => ["Object self", "Int one", "Str two"]
        method_two_fallback => ["Object self", "..."]
    }:

    my $multi = new Params::Signature::Multi();

    # all positional params
    sub_one(1,"hi")

    sub sub_one
    {
        return $multi->dispatch(\@_, 
                        [ 
                            { signature => $prototypes{sub_one_a}, call => \&_sub_one_a},
                            { signature => $prototypes{sub_one_b}, call => \&_sub_one_a}
                            { signature => $prototypes{sub_one_fallback}, call => \&_sub_one_fallback}
                        ]);
    }

    sub _sub_one_a
    {
        my $params = $multi->validator->validate(\@_, $prototypes{sub_one_a});
        ...
    }
    sub _sub_one_b
    {
        my $params = $multi->validator->validate(\@_, $prototypes{sub_one_b});
        ...
    }
    sub _sub_one_fallback
    {
        # perhaps return a default value or print an error
        ...
    }

    $self->method_two(1,"hi");

    sub method_two
    {
        return $multi->dispatch(\@_, 
                        [ 
                            { signature => $prototypes{method_two_a}, call => \&_method_two_a},
                            { signature => $prototypes{method_two_b}, call => \&_method_two_b}
                            { signature => $prototypes{method_two_fallback}, call => \&_method_two_fallback}
                        ]);
    }

    # determine which signature matches and handle the rest locally
    sub my_accessor
    {
        my $self = shift;
        my ($idx, $id) = $multi->resolve(
                    params => \@_, 
                    signatures => [
                        { id => "set",  signature => ["Str key", "Str value"]},
                        { id => "get", signature => ["Str key"], }
                        # without a fallback, Carp::confess is called when nothing matches
                        { id => "fallback", signature => ["..."], }
                    ],
                    param_tyle => "positional"
                );
        if ($id eq "set")
        {
            return $self->{$_[0]} = $_[1];
        }
        elsif ($id eq "get")
        {
            return $self->{$_[0]};
        }
        else # fallback
        {
            shout("You didn't give me a key!");
        }

    }

=head1 DESCRIPTION

Many object-oriented languages, including Perl 6, allow you to define multiple methods with the same name but a different subroutine signature.  At run time, the interpreter determines which signature matches the parameters being passed to the method and calls the corresponding method.  Params::Signature::Multi builds on Params::Signature and uses it to select the best matching subroutine and call it accordingly.  Note that Params::Signature::Multi treats everything as a subroutine, since (in perl 5) even methods are actually just subroutines.  

In its simplest form, you simply call Params::Signature::Multi's L</dispatch> method with your parameters and a list of signature specifications:

    return $multi->dispatch(\@_, 
                [
                    { signature => ["Int one", "Str two = 'A default value'", "Undef|Str three?], call => \&call_me},
                    { signature => ["Int one"], call => \&or_call_me}
                    { signature => ["..."], call => \&call_me_fallback}
                ],
                # default parameter style is 'positional' (or whatever was passed to
                # '$multi' object's constructor) ...
                # force parameter style to 'named' 
                "named"
                );

    return $multi->dispatch(
                    params => \@_, 

                    signatures => [
                            { signature => ["Int one", "Str two = 'A default value'", "Undef|Str three?], call => \&call_me},
                            { signature => ["Int one"], call => \&or_call_me}
                            { signature => ["..."], call => \&call_me_fallback}
                        ],

                    # default parameter style is 'positional' (or whatever was passed to
                    # '$multi' object's constructor) ...
                    # force parameter style to 'named' 

                    param_style => "named",
                    
                    on_fail => \&my_error_handler
                );


A signature is a list of parameter definitions as defined in L<Params::Signature>.  Note that default values defined in the signature are ignored, and therefore not set, by L</dispatch> or L</resolve>.  These methods check parameter type constraints only.

The list of signatures is defined in the same order used by L</dispatch> and L</resolve> to evaluate signatures and identify the first matching signature.  The most specific signature should be first with more general signatures following it.  The first signature that matches is considered the "best match", even if subsequent signatures would also match.  A "fallback" signature which accepts anything (via the "..." pseudo-parameter) can be used to handle situations where the the parameters don't match any signature.

=cut


=head1 METHODS

All functionality is implemented via object (or class) methods.  You can use L<dispatch> and L</resolve> as Params::Signature::Multi class methods rather than constructing a module or application-specific Params::Signature::Multi object.

=head2 new

    new Params::Signature::Multi(
            param_style => "positional",
            fuzzy => 1,
            on_fail => sub { oops($_[0]) },
            normalize_keys => sub { lc $_[0] },
            called => "My::Module"
        );

The "new" method takes the same parameters as those passed to L</Params::Signature>'s "new" method.


=cut

=head2 resolve

    my ($idx, $id) = $multi->resolve(
                    params => \@_, 
                    signatures => [
                        { id => "set",  signature => ["Str key", "Str value"]},
                        { id => "get", signature => ["Str key"], }
                    ],
                    param_tyle => "positional"
                );


Only resolve which signature actually matches the parameters passed to a subroutine.

B<params>: A reference to an array of parameters passed to the calling subroutine, typically C<\@_>.  This array is left alone and may be used after the call to resolve.

B<signatures>: A list of signatures in the order in which resolve will attempt to find a matching signature.  If signatures are similar, more specific signatures (those that have more parameters or stricter type constraints) should appear first, with the least specific signature at the end.  See the L<Limitations and Caveats> section below for additional considerations.

=over 4

=item * B<signature>:

The actual subroutine signature.

=item * B<id>:

Optional ID assigned to a signature.

=back

B<param_style>: Explicitly set the parameter style used to evaluate parameters.

B<on_fail>: Override Carp::confess as the subroutine that gets called when a failure occurs.  This parameter must be passed to C<resolve> as a named parameter.

B<Return Value>:

In scalar context, the method returns the index of the signature that was selected.  If the optional "id" value is set in the C<signatures> section, the "id" is returned instead of the index.  In list context, the method returns the index and id.

=cut

=head2 dispatch

    return $multi->dispatch(
            params => \@_, 
            signatures => [
                { signature => ["Int one", "Str two = 'A default value'", "Undef|Str three?], call => \&_call_me},
                { signature => ["Int one"], call => \&_or_call_me}
            ],
            param_tyle => "positional"
        );


Determine which signature matches the parameters in C<params> and call the corresponding subroutine.

B<params>: A reference to an array of parameters passed to the calling subroutine, typically C<\@_>.  This array is left alone and may be used after the call to dispatch.

B<signatures>: A list of signatures in the order in which resolve will attempt to find a matching signature.  If signatures are similar, more specific signatures (those that have more parameters) should appear first, with the least specific signature at the end.  

=over 4

=item * B<signature>:

The actual subroutine signature.

=item * B<call>:

The subroutine to call if the C<signature> matches.  The list assigned to C<params> is passed to the subroutine as C<@_>.

=back

B<param_style>: Explicitly set the parameter style used to evaluate parameters.

B<on_fail>: Override Carp::confess as the subroutine that gets called when a failure occurs.  This parameter must be passed to C<dispatch> as a named parameter.

B<Return Value>:

The return value of the subroutine that was called or undef if nothing was called.


=cut


=head1 PERFORMANCE

A Params::Signature object is used to cache the parsed form of each signature evaluated.  Re-using the same object to evaluate subroutine parameters eliminates the need to parse the signature every time.  Using a Params::Signature::Multi singleton per module or application is recommended for reducing the amount of time it takes to evaluate parameters.


=head1 LIMITATIONS AND CAVEATS

=head2 Resolution Method

This is a "poor man's" method for supporting multi methods and subroutines.  The current means of selecting the first signature that matches rather than evaluating all of them and picking the "best match" may not be the best approach.  Evaluating signatures in list order has the advantage of giving the programmer some control over the order of evaluation, thus providing predictable results.  This remains a "best effort" and is not intended to emulate more comprehensive systems.

=head2 Inheritance

Inheritance is not supported directly.  The initial method that gets called is resolved by perl.  Once the method is called, other methods in the method's package are available as values for use with C<call>.  However, there is no easy way to know if a C<call> method has been overriden in a child object.  If you really need proper inheritance and multi-method, you probably should be using Moose with MooseX and not this package.  

A possible solution to inheritance:

    my $self = shift;
    return $multi->dispatch(
                params => \@_, 
                signatures => [
                    # instead of this:
                    # { signature => ["Int one", "Str two", call => \&call_me},
                    # do this:
                    { signature => ["Int one", "Str two", call => sub { $self->call_me(@_) },
                    # not this ...
                    # { signature => ["Int one"], call => \&or_call_me}
                    # try this ...
                    { signature => ["Int one"], call => sub { $self->or_call_me(@_) }
                ],
                param_tyle => "positional"
                );

This method relies on C<$self> being re-evaluated each time the anonymous subroutine gets assigned as the C<call> parameter.  This also requires that you C<shift> off C<$self> from C<@_>.  Give it a try.  Your mileage may may vary.

=head2 Type Constraints

Keep in mind that some type constraints are somewhat interchangable because of the way perl itself handles data.  For example, an integer value stored in a perl scalar will automatically behave like a string in the right context.  A string that contains an integer value can be treated like an integer.  In perl, there is no way to distinguish between one type of value versus the other.  The type constraints perform tests to see if a value has certain characteristics.  For example, the test for an Int is more specific (value must contain only digits) than the type constraint tests for a Str. If the only difference between two signatures is an Int versus an Str parameter, place the signature with the Int constraint before the signature with the Str value.  The stricter test for an Int will fail for anything that is not an integer value, but the test for a Str will accept a numeric value as well.

    sub int_or_str
    {
        my ($idx, $id) = $multi->resolve(
                    params => \@_, 
                    signatures => [
                        { id => "is_int",  signature => ["Int arg_one"]},
                        { id => "is_str",  signature => ["Str arg_one"]},
                    ],
                    param_tyle => "positional"
                );
        ...
    }

    int_or_str(123);   # resolves to "is_int"
    int_or_str("123"); # also resolves to "is_int"!
    int_or_str("hi"); # resolves to "is_str"
    
Using named, rather than positional, parameters can help eliminate some ambiguity, provided you use a different parameter name for the different types of values.  If, like in the example above, both signatures use the same name (arg_one), using named parameters doesn't have any advantage over positional parameters.  Changing the names to C<int_arg> and C<str_arg> and using named parameters clears things up:

    sub int_or_str
    {
        my ($idx, $id) = $multi->resolve(
                    params => \@_, 
                    signatures => [
                        { id => "is_int",  signature => ["Int int_arg"]},
                        { id => "is_str",  signature => ["Str str_arg"]},
                    ],
                    param_tyle => "named"
                );
        ...
    }

    int_or_str(int_arg => 123);   # resolves to "is_int"
    int_or_str(int_arg => "123"); # also resolves to "is_int"!
    int_or_str(str_arg => "hi");  # resolves to "is_str"
    int_or_str(str_arg => "123"); # also resolves to "is_str"!


=cut

=head1 AUTHOR

Sandor Patocs, C<< <perl at patocspack.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-params-signature-multi at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Params-Signature>.  
I will be notified, and then you'll automatically be notified of progress on your bug as I make changes.


=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Params::Signature::Multi


You can also look for information at:

=over 4

=item * RT: CPAN's request tracker (report bugs here)

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=Params-Signature>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/Params-Signature>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/Params-Signature>

=item * Search CPAN

L<http://search.cpan.org/dist/Params-Signature/>

=back


=head1 ACKNOWLEDGEMENTS


=head1 SEE ALSO

L<MooseX::Method::Signatures>, L<MooseX::MultiMethods>, L<Method::Signatures>, L<Perl6::Signature>


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Sandor Patocs.

This program is distributed under the terms of the Artisitic License (2.0)


=cut

1; # End of Params::Signature
