package Params::Signature;
# TODO: 
#       write documentation
#       post on github
#       test in SplatDB
#       if it works for SplatDB, put on CPAN
use 5.006;
use strict;

use Carp;
use Class::Load;
use Scalar::Util;
use Data::Dumper;

our $VERSION = "0.01";

my $CUTOFF_SYMBOL = "named:";
my $EXTRA_SYMBOL = "...";

my $ASSIGN_PARAM_SYMBOL = "<=";
my $ASSIGN_SYMBOL = "=";
my $DEPENDS_SYMBOL = "<<";

my $ASSIGN_NONE = 0;
my $ASSIGN_PARAM = 1;
my $ASSIGN_LITERAL = 2;
my $ASSIGN_SUB = 3;

my $POSITIONAL = "positional";
my $NAMED = "named";
my $MIXED = "mixed";
my $DEFAULT_PARAM_STYLE = $POSITIONAL;

my $FAILED = 0;
my $OK = 1;
my $FIELD_SEPARATOR = \0x1c;

my $debug = 0;

my $class_default = undef;
*class_default = sub { ($class_default) ? $class_default : ($class_default = new Params::Signature(param_style => $DEFAULT_PARAM_STYLE, fuzzy => 0, register_builtins => 1, on_fail => \&confess, called => "Params::Signature:")) };

sub new
{
    my @new_params = qw(param_style on_fail called normalize_keys fuzzy);
    my $param;
    my $class = shift;
    my %params = @_;
    my $self = {};

    bless $self, $class;

    # this should only be done for the class $class_default object
    if ($params{register_builtins})
    {
        $self->_register_builtins;
    }
    
    foreach $param (@new_params)
    {
        $self->{$param} = $params{$param};
    }
    return $self;
}


# TODO: need to validate arguments to register_type using our own validate()
sub register_type
{
    my $self;
    my $parent;
    my $child;
    my $inline_as;
    my @subs;
    my $ref_type;
    my $on_fail;

    $self = (ref($_[0])) ? shift @_ : (shift @_ and class_default());

    #my %params = validate_with(params => \@_, spec => $register_type_spec);
    # TODO: validate params using our own validate()
    my %params = @_;
    print_debug("register_type:", %params);
    if (defined($params{parent}) && (!defined($self->{type}{$params{parent}})))
    {
        $on_fail = (defined($self->{on_fail})) ? $self->{on_fail} : $class_default->{on_fail};
        $on_fail->("Parent type '$params{parent} 'not defined for type '$params{name}'");
    }
    elsif (defined($params{parent}))
    {
        $params{parent_obj} = $self->{type}{$params{parent}};
    }

    if (defined($params{inline_as}))
    {
        print_debug("make inline for $params{name}\n");
        INLINE:
        {
            $inline_as = "(" . $params{inline_as}->() .  ")";
            for($child = \%params;
                defined($child->{parent_obj});
                $child = $parent)
            {
                $parent = $child->{parent_obj};

                print_debug("get inline_as fo parent $parent->{name}");

                # detect break in the chain of inline_as; every
                # parent should have one otherwise, the where subs
                # have to be used (ie. can't have an incomplete inlining
                # of tests)
                if (!$parent->{inline_as} && defined($parent->{parent_obj}->{inline_as}))
                {
                    print_debug("break in the chain!\n");
                    $inline_as = undef;
                    last INLINE;
                }
                $inline_as = "(" . $parent->{inline_as}->() . ") && " . $inline_as;
            }
            print_debug("$params{name} inline_as: $inline_as");
            $params{inline_sub} = eval ("sub {$inline_as}; ");
            print_debug("inline_sub is: " . ref($params{inline_sub}));
        }
    }

    # store list of 'where' subs for faster execution later
    push(@subs, $params{where});
    for($child = \%params;
        defined($child->{parent_obj});
        $child = $parent)
    {
        $parent = $child->{parent_obj};
        push(@subs, $parent->{where});
    }
    $params{where_subs} = [reverse @subs];

    $self->{type}{$params{name}} = \%params;
}

sub register_class
{
    my $self = (ref($_[0])) ? shift @_ : class_default();
    my %params = @_;
    my $class_name = $params{name};
    print_debug("register_class", %params);
    $self->register_type(name => $class_name, parent => "Object", 
                        where => sub { $_[0]->isa($class_name) },
                        inline_as => sub { '$_[0]->isa(' . "$class_name)" });
}

sub register_role
{
    my $self = (ref($_[0])) ? shift @_ : class_default();
    my %params = @_;
    my $role_name = $params{name};
    print_debug("register_role", %params);
    $self->register_type(name => $role_name, parent => undef, 
                        where => sub { $_[0]->DOES($role_name) },
                        inline_as => sub { '$_[0]->DOES(' . "$role_name)" });
}

# Type definitions shamelessly stolen from Moose::Util::TypeConstraints::Builtins
sub _register_builtins
{
    my $self = shift;
    $self->register_type(name => "Any", parent => undef, where => sub {1}, inline_as => sub { '1' });
    $self->register_type(name => "Item", parent => "Any", where => sub {1}, inline_as => sub { '1' });

    $self->register_type(name => 'Undef',
        parent => 'Item',
        where => sub { !defined($_[0]) },
        inline_as => sub { '!defined($_[0])' }
    );

    $self->register_type(name => 'Defined',
        parent => 'Item',
        where => sub { defined($_[0]) },
        inline_as => sub { 'defined($_[0])' }
    );

    $self->register_type(name => 'Bool',
        parent => 'Item',
        where => sub { !defined($_[0]) || $_[0] eq "" || "$_[0]" eq '1' || "$_[0]" eq '0' },
        inline_as => sub {
            '('
                . '!defined($_[0]) '
                . '|| $_[0] eq "" '
                . '|| ($_[0]."") eq "1" '
                . '|| ($_[0]."") eq "0"'
            . ')'
        });

    $self->register_type(name => 'Value',
        parent => 'Defined',
        where => sub { !ref($_[0]) },
        inline_as => sub {
             ' !ref($_[0])'
        });

    $self->register_type(name => 'Ref',
        parent => 'Defined',
        where => sub { ref($_[0]) },
            # no need to call parent - ref also checks for definedness
        inline_as => sub { 'ref($_[0])' });

    $self->register_type(name => 'Str',
        parent => 'Value',
        where => sub { ref(\$_[0]) eq 'SCALAR' || ref(\(my $val = $_[0])) eq 'SCALAR' },
        inline_as => sub {
             ' ('
                . 'ref(\\$_[0]) eq "SCALAR"'
                . ' || ref(\\(my $val = $_[0])) eq "SCALAR"'
            . ')'
        });

    $self->register_type(name => 'Num',
        parent => 'Str',
        where => sub { Scalar::Util::looks_like_number($_[0]) },
        inline_as => sub {
            # the long Str tests are redundant here
             ' Scalar::Util::looks_like_number($_[0])'
        });

    $self->register_type(name => 'Int',
        parent => 'Num',
        where => sub { (my $val = $_[0]) =~ /\A-?[0-9]+\z/ },
        inline_as => sub {
             ' (my $val = $_[0]) =~ /\A-?[0-9]+\z/'
        });

    $self->register_type(name => 'ArrayRef',
        parent => 'Ref',
        where => sub { ref($_[0]) eq 'ARRAY' },
        inline_as => sub { 'ref($_[0]) eq "ARRAY"' });

    $self->register_type(name => 'HashRef',
        parent => 'Ref',
        where => sub { ref($_[0]) eq 'HASH' },
        inline_as => sub { 'ref($_[0]) eq "HASH"' });

    $self->register_type(name => 'CodeRef',
        parent => 'Ref',
        where => sub { ref($_[0]) eq 'CODE' },
        inline_as => sub { 'ref($_[0]) eq "CODE"' });

    $self->register_type(name => 'RegexpRef',
        parent => 'Ref',
        where => sub { ref($_[0]) eq 'Regexp' },
        inline_as => sub { 'ref($_[0]) eq "Regexp"' });

    $self->register_type(name => 'GlobRef',
        parent => 'Ref',
        where => sub { ref($_[0]) eq 'GLOB' },
        inline_as => sub { 'ref($_[0]) eq "GLOB"' });

    # NOTE: scalar filehandles are GLOB refs, but a GLOB ref is not always a
    # filehandle
    $self->register_type(name => 'FileHandle',
        parent => 'Ref',
        where => sub {
            (ref($_[0]) eq "GLOB" && Scalar::Util::openhandle($_[0]))
         || (Scalar::Util::blessed($_[0]) && $_[0]->isa("IO::Handle"));
        },
        inline_as => sub {
            '(ref($_[0]) eq "GLOB" '
            . '&& Scalar::Util::openhandle($_[0])) '
            . '|| (Scalar::Util::blessed($_[0]) '
            . '&& $_[0]->isa("IO::Handle"))'
        });

    $self->register_type(name => 'Object',
        parent => 'Ref',
        where => sub { Scalar::Util::blessed($_[0]) },
        inline_as => sub { 'Scalar::Util::blessed($_[0])' });

    $self->register_type(name => 'ClassName',
        parent => 'Str',
        where => sub { Class::Load::is_class_loaded($_[0]) },
            # the long Str tests are redundant here
        inline_as => sub { 'Class::Load::is_class_loaded($_[0])' });

}

sub type_check
{
    my $self = (ref($_[0])) ? shift @_ : class_default();
    my $type_spec = shift;
    my $value = shift;
    my @all_types;
    my $type_passed = 0;
    my $type;
    my $check_in;

    @all_types = ($type_spec =~ /\|/) ? split(/\|/, $type_spec) : ($type_spec);

    # check object for object-specific types
    foreach $type (@all_types)
    {
        # look in self if type is defined, otherwise check in
        # global type definitions
        print_debug("type_check: $type($value)");
        $check_in = (defined($self->{type}{$type})) ? $self : Params::Signature->class_default;
        $type_passed = (defined($check_in->{type}{$type}->{inline_sub})) ? $check_in->{type}{$type}->{inline_sub}->($value) : ($check_in->_type_check_via_where($type_spec, $value));
        if ($type_passed) { last; }
    }
    return ($type_passed) ? 1 : 0;
}

sub _type_check_via_where
{
    my $self = shift;
    my $type_spec = shift;
    my $value = shift;
    my @all_types;
    my $type_passed = 0;
    my $type;
    my $check_in;
    my $where_sub;

    print_debug("_type_check_via_where: $type_spec => $value");
    @all_types = ($type_spec =~ /\|/) ? split(/\|/, $type_spec) : ($type_spec);

    # check object for object-specific types
    foreach $type (@all_types)
    {
        $check_in = (defined($self->{type}{$type})) ? $self : Params::Signature->class_default;

        # should a missing type be:
        # 1. a fatal error (call confess())
        # 2. result in a 'false' return value (failed type check)
        # 3. be silently ignored
        # NOTE: perhaps this should be configurable?
        if (!defined($check_in->{type}{$type})) { next; }

        foreach $where_sub (@{$check_in->{type}{$type}->{where_subs}})
        {
            if (!($type_passed = $where_sub->($value))) { last; }
        }
    }
    return $type_passed;
}

#spec("Str name!",  "Str alias < name", "ArrayRef options = []", "HashRef h = { call_a_sub }", "CustomRelationType relation = '1-N'", "Str other ~ /[a-z]/")

sub _build_signature_info
{
    my $ARG_MAX = 4;
    my $self = shift;
    my $arg;
    my @parts;
    my @aliases;
    my $alias;
    my $type;
    my $name;
    my $optional;
    my $indicator;
    my $indicator_value;
    my $type_info;
    my $spec;
    my $idx = -1;
    my @signature_spec = ();
    my %signature_info = ();
    my $ok = 1;
    my $msg;
    my $cutoff;
    my $flag;

    foreach $arg (@{$_[0]})
    {
        $spec = {};
        ($type, $name, $indicator, $indicator_value) = split(/\s+/, $arg, $ARG_MAX);
        print_debug("Process arg: ", "arg: $arg", "type: $type", "name: $name", "indicator: $indicator", "indicator_value: $indicator_value");
        $idx++;
        $spec->{type} = $type;
        $spec->{idx} = $idx;
        $spec->{name} = $name;
        $spec->{indicator} = $indicator;
        $spec->{indicator_value} = $indicator_value;
        if ($type eq $CUTOFF_SYMBOL)
        {
            print_debug("is cutoff");
            #$name = $CUTOFF_SYMBOL;
            #$spec->{name} = $CUTOFF_SYMBOL;
            #$spec->{is_cutoff} = 1;
            #$spec->{idx} = $idx;
            $signature_info{positional_cutoff} = $idx;
            #$signature_info{map}{$name} = $spec;
            $idx--;
            next;
        }
        elsif ($type eq $EXTRA_SYMBOL)
        {
            print_debug("is extra");
            $name = $EXTRA_SYMBOL;
            $spec->{name} = $EXTRA_SYMBOL;
            $spec->{is_extra} = 1;
            $spec->{idx} = $idx;
            $signature_info{extra_cutoff} = $idx;
            $signature_info{map}{$name} = $spec;
            $idx--;
            next;
        }

        # perl6-ish variable name
        if (((index($name, ":\$") == 0) ||
                (index($name, ":") == 0)) &&
            (!defined($signature_info{positional_cutoff})))
        {
            $signature_info{positional_cutoff} = $idx;
        }

        if ((index($name, "\$") == 0) &&
            (defined($signature_info{positional_cutoff})))
        {
            $ok = 0;
            $msg = "Positional parameter $name cannot appear after a named parameter";
            last;
        }
        $name =~ s/^[:\$]*//;

        $name =~ s/([\!\?])$//;
        $flag = $1;
        if (index($name, "|") != -1)
        {
            @aliases = split(/\|/, $name);
            $name = $aliases[0];
            foreach $alias (@aliases)
            {
                $signature_info{map}{$alias} = $spec;
            }
        }

        # only actual argument specifications get processed below
        if (defined($flag) && $flag eq "?")
        {
            $spec->{optional} = 1;
            if (!defined($signature_info{pos_optional_start}))
            {
                $signature_info{pos_optional_start} = $idx;
            }
        }
        else
        {
            # required by default
            $spec->{optional} = 0;
            $signature_info{required}{$name} = $spec;
            if (!$signature_info{positional_cutoff})
            {
                $signature_info{positional_last_required} = $idx;
            }
        }
        $spec->{name} = $name;

        print_debug("Normal arg: $name");

        if ($indicator eq $ASSIGN_SYMBOL)
        {
            if ($indicator_value =~ /[\&\{\[]/)
            {
                print_debug("Assign sub for default: $indicator_value");
                $spec->{default_type} = $ASSIGN_SUB;
                $spec->{default} = eval "sub { $indicator_value }";

            }
            else
            {
                print_debug("Assign literal for default: $indicator_value");
                $spec->{default_type} = $ASSIGN_LITERAL;
                $spec->{default} = eval "$indicator_value;";
            }
        }
        elsif ($indicator eq $ASSIGN_PARAM_SYMBOL)
        {
            print_debug("Assign param for default: $indicator_value");
            $spec->{default_type} = $ASSIGN_PARAM;
            $spec->{default} = $indicator_value;
            if (!defined($signature_info{map}{$indicator_value}))
            {
                $ok = 0;
                $msg = "$indicator_value must appear before $name in signature";
                last;
            }
        }
        elsif ($indicator eq $DEPENDS_SYMBOL)
        {
            # NOTE: this should really only be set for an optional value;
            #       something that is required (or has a default value) will
            #       always cause the 'depends' fields to be required
            #       which makes defining this dependency redundant
            #       and unnecessary; optional fields should depend on
            #       an optional field
            $spec->{default_type} = $ASSIGN_NONE;
            my $iv = $indicator_value;
            $iv =~ s/,/ /;
            $iv =~ s/^\[(.*)\]$/qw(\1)/;
            @{$spec->{depends}} = eval $iv;
            if (ref($spec->{depends}) ne "ARRAY")
            {
                $ok = 0;
                $msg = "Dependency $indicator_value for $name must be an array";
                last;
            }
            print_debug("Configure dependency: $indicator_value");
        }
        elsif (length($indicator))
        {
            $ok = 0;
            $msg = "$indicator not understood after $name in signature. Missing space?";
            last;
        }
        $signature_spec[$idx] = $spec;
        $signature_info{map}{$name} = $spec;
    }
    $signature_info{signature_spec} = \@signature_spec;

    if (defined($signature_info{extra_cutoff}) && 
        ($signature_info{extra_cutoff} != scalar @signature_spec))
    {
        $ok = 0;
        $msg = "Extra parameter indicator must be the last item in the signature";
    }

    # if we have a positional cutoff in the signature, we can verify
    # that required arguments do no appear after optional ones;
    # otherwise, the sanity of the signature cannot be determined here
    # because there is no way to know if the subroutine will allow positional, 
    # named or either style of parameters when it's actually called
    #   ... setting the first arg in signature to ">>>" lets us know the sub
    #       will be called with named parameters only
    #   ... setting the last arg in signature to ">>>" lets us know the sub
    #       will be called with positional parameters only
    $cutoff = $signature_info{positional_cutoff};
    foreach $idx (0 .. $signature_info{positional_cutoff} - 1)
    {
        if ($signature_spec[$idx]->{optional})
        {
            $cutoff = $signature_spec[$idx]->{optional};
        }
        if ((!$signature_spec[$idx]->{optional}) && ($idx > $cutoff))
        {
            $ok = 0;
            $msg = "A required positional parameter ($signature_spec[$idx]->{name}) cannot appear after an optional one";
            last;
        }
    }

    print_debug("done: sub _build_signature_info");
    return (\%signature_info, $ok, $msg);
}

# called as: validate(\@_, [ spec ]);
# called as: validate(
#        params => \@_,
#        signature => [ spec ],
#        normalize_keys => sub{},
#        on_fail => sub{},
#        stack_skip => [0-9],   # not actually implemented
#        allow_extra => [0|1],  # deprecated ... use "..." in signature
#        fuzzy => [0|1],
#        called => "msg",
#        callbacks => { field => { cb_name => sub{} }
#        );
sub validate
{
    print_debug("start: sub validate", Dumper(\@_));
    # param_style is left out of @validate_params on purpose
    my @validate_params = qw(signature params normalize_keys fuzzy on_fail called callbacks);
    my $self = (ref($_[0])) ? shift @_ : (shift @_ and class_default());
    my %params;  # parameters passed to validate()
    my $param;   # reference to a parameter in %params
    my @local_caller_params;  # local copy of caller's params
    my $caller_params;  # reference to caller's params
    my $param_count;  # number of parameters passed to caller
    my $param_left;  # number of parameters left to process
    my $signature_param_count; # number of params defined in signature
    my $positional_cutoff; # magic marker separating positional and named arguments
    my $extra_cutoff; # magic marker separating positional from extra args

    my $idx;
    my $max;
    my $signature_spec;
    my $signature_info;
    my $on_fail;
    my $fuzzy;
    my $called;
    my $normalize_keys;
    my $ok;
    my $msg;
    my $value;
    my $key;
    my @return_list;
    my %return_hash;
    my @required;
    my $param_style;
    my $check_only;

    print_debug("\$class_default:", Dumper($class_default));
    print_debug("class_default():", Dumper(class_default()));

    # configuration options

    # given just the params and signature
    print_debug("\@_ has " . scalar @_ );
    if (scalar @_ == 2)
    {
        print_debug("Assign 0 and 1 to params and signature");
        $params{params} = $_[0];
        $params{signature} = $_[1];
    }
    else
    {
        %params = @_;
    }

    print_debug("validate: \$self=", ref($self));
    print_debug("validate params:", Dumper(%params));

    # check for validation defaults
    foreach $param (@validate_params)
    {
        if (!$params{$param})
        {
            (defined($self->{$param})) ? print_debug("using \$self->$param") : print_debug("using class->$param");
            $params{$param} = (defined($self->{$param})) ? $self->{$param} : $class_default->{$param};
            print_debug ("Assign $param default value: $params{$param}");
        }
    }

    $on_fail = $params{on_fail};
    $fuzzy = $params{fuzzy};
    $called = $params{called};
    $normalize_keys = $params{normalize_keys};
    $check_only = $params{check_only};

    if (!defined($params{params}))
    {
        print_debug("$called Missing the list of parameters to validate");
        $on_fail->("$called Missing the list of parameters to validate");
        return;
    }

    if (ref($params{params}) ne "ARRAY")
    {
        print_debug("$called The list of parameters to validate is not an array reference, it's a " . ref($params{params}));
        $on_fail->("$called The list of parameters to validate is not an array reference, it's a " . ref($params{params}));
        return;
    }

    # make our own copy of the calling subroutine's params so caller
    # can still look at it's own @_ for original params
    $caller_params = $params{params};
    @local_caller_params = @{$params{params}};
    $params{params} = \@local_caller_params;

    # init values used later
    $param_count = scalar @{$params{params}};
    ($signature_info, $ok, $msg) = $self->_get_signature_info($params{signature});
    if (!$ok)
    {
        $on_fail->("$called $msg!!");
        return;
    }

    print_debug("Signature info", Dumper($signature_info));

    $signature_spec = $signature_info->{signature_spec};
    $signature_param_count = scalar @{$signature_spec};
    $positional_cutoff = $signature_info->{positional_cutoff};
    $extra_cutoff = $signature_info->{extra_cutoff};
    @required = (keys %{$signature_info->{required}});

    # determine what style of parameters we have, positional, named or mixed
    # possibile values for $positional_cutoff:
    # !defined => use param style (POSITIONAL or NAMED)
    # defined => process as MIXED

    # determine param style 
    ($param_style, $ok, $msg) = $self->_get_param_style($params{params}, $params{param_style}, $fuzzy, $normalize_keys, $signature_info);
    if (!$ok)
    {
        $on_fail->("$called $msg");
        return;
    }

    print_debug("Param style: $param_style");

    # NOTE: if a positional_cutoff exists, the signature has already been
    #       checked in _build_signature_info for proper required/optional ordering,
    #       otherwise, we have to check here now that we know the param style is
    #       positional and not named
    if (($param_style == $POSITIONAL) &&
        (defined($signature_info->{pos_optional_start})) &&
        ($signature_info->{positional_last_required} > $signature_info->{pos_optional_start}))
    {
        $on_fail->("A required positional parameter ($signature_spec->[$signature_info->{positional_last_required}]->{name}) cannot appear after an optional one");
    }

    if (($param_style eq $POSITIONAL) || ($positional_cutoff > 0))
    {
        # process args until we reach a cutoff marker or the end
        $max = ($positional_cutoff || $extra_cutoff || $signature_param_count);

        # params are associated with the corresponding positional argument
        # in the signature specification
        foreach ($idx = 0; $idx < $max; $idx++)
        {
            if ((defined($signature_info->{pos_optional_start})) &&
                ($idx >= $param_count) &&
                ($idx >= $signature_info->{pos_optional_start}))
            {
                # optional parameters are missing, so there's nothing to check
                next;
            }
            $value = $params{params}[$idx];
            $param = $signature_spec->[$idx]{name};

            # NOTE: * if $value is undef and it's a required value, validate_arg
            #       will assign the default value, if any; 
            #       * an 'undef' value MUST be passed in if named parameters are
            #       also passed in otherwise the named parameters will be 
            #       processed as positional values leading to unexpected 
            #       results or a validation error
            #       * fields that are processed because of a 'depends'
            #       clause should not be processed twice
            if (!defined($return_hash{$param}))
            {
                ($value, $ok, $msg) = $self->validate_arg(value => $value, spec => $signature_spec->[$idx], info => $signature_info, set_values => \%return_hash, callbacks => $params{callbacks}, original_values => $caller_params, check_only => $check_only);
            }
            if (!$ok)
            {
                $on_fail->("$called $msg");
                return;
            }

            print_debug("idx: $idx");

            $return_list[$idx] = $value;
            $return_hash{$param} = $value;

            # optional fields become required if the field they
            # depend on is present; they are processed immediately
            # and are not re-processed above
            if ($signature_info->{map}{$param}->{depends})
            {
                foreach $param (@{$signature_info->{map}{$param}->{depends}})
                {
                    $param = $signature_info->{map}{$param}->{name};
                    if (!defined($return_hash{$param}))
                    {
                        my $dep_idx = $signature_info->{map}{$param}->{idx};
                        $value = $params{params}[$dep_idx];
                        ($value, $ok, $msg) = $self->validate_arg(value => undef, spec => $signature_info->{map}{$param}, info => $signature_info, set_values => \%return_hash, callbacks => $params{callbacks}, original_values => $caller_params, check_only => $check_only);
                        if (!$ok)
                        {
                            $on_fail->("$called $msg");
                            return;
                        }
                        $return_list[$dep_idx] = $value;
                        $return_hash{$param} = $value;
                    }
                }
            }
        }

        # if we actually have required fields, then make sure 
        # all of them are set (either to a param value or default)
        if ((defined($signature_info->{positional_last_required})) 
            && ($#return_list < $signature_info->{positional_last_required}))
        {
            my $count = $signature_info->{positional_last_required} - $#return_list;
            #my $s = Dumper(\@return_list) . "\n" . Dumper(\%return_hash);
            #$on_fail->("$called $count required fields missing ($signature_info->{positional_last_required} / $s)");
            $on_fail->("$called $count required fields missing");
        }

        print_debug("idx: $idx, extra_cutoff: $extra_cutoff");
        # if we are at extra cutoff, stick remaining args in return value;
        # not sure how to add extra *positional* params to return_hash;
        # what should I use as a key? extra parameters should probably
        # not be used with mixed param_style ... all positional or all named
        if (defined($extra_cutoff) && $idx == $extra_cutoff)
        {
            for ($idx; $idx < scalar @{$params{params}}; $idx++)
            {
                push(@return_list, $params{params}[$idx]);
            }
        }

        # remove positional parameters from local copy of parameters
        splice (@{$params{params}}, 0, $idx);

        if (($param_style eq $POSITIONAL) && scalar @{$params{params}})
        {
            $on_fail->("$called Encountered unexpected extra parameter near '" . $params{params}[0] . "' at index $idx");
            return;
        }
    }
    print_debug("return_list: ", Dumper(\@return_list), "", "return_hash: ", Dumper(\%return_hash));
    print_debug("remaining params: " . Dumper(\@{params{params}}));
    print_debug("param_style: $param_style, param_left: $param_left, param_count: $param_count, idx: $idx");

    # handle remaining (named) arguments
    if (($param_style eq $NAMED) || ($param_style eq $MIXED))
    {
        $param_left = $param_count - $idx;
        print_debug("param_left: $param_left, param_count: $param_count, idx: $idx");

        if ((scalar @{$params{params}} == 1)
            && (ref($params{params}[0]) eq "HASH"))
        {
            print_debug("convert hash to params");
            # turn hash into array, for while loop below 
            my @named_params = ();
            map { push(@named_params, $_, $params{params}[0]{$_}) } keys %{$params{params}[0]};
            $params{params} = \@named_params;
            $param_count = scalar @named_params;
            $param_left = $param_count;
            $idx = 0;
            print_debug("new named params: " . Dumper(\@{params{params}}));
        }

        if (!$extra_cutoff  && (($idx + ($param_left/2)) > $signature_param_count))
        {
            $on_fail->("$called Encountered unexpected extra parameter");
            return;
        }

        # remove param name/value pairs from remaining params
        while (($param_left > 0) && (($param, $value) = splice(@{$params{params}}, 0, 2)))
        {
            $idx++;
            $param_left -= 2;
            # must normalize keys here because signature contains
            # canonical parameter name; the two must match
            print_debug("normalize param: $param");
            $param = ($normalize_keys) ? $normalize_keys->($param) : $param;

            # get name in case $param is an alias
            $param = (defined($signature_info->{map}{$param})) ? $signature_info->{map}{$param}{name} : $param;

            print_debug("final param name: $param");
            print_debug("value: $value");

            if (!$extra_cutoff || $idx < $extra_cutoff) 
            {
                if (!defined($signature_info->{map}{$param}))
                {
                    $on_fail->("$called Named parameter '$param' does not exist in the signature");
                    return;
                }

                ($value, $ok, $msg) = $self->validate_arg(value => $value, spec => $signature_info->{map}{$param}, info => $signature_info, set_values => \%return_hash, callbacks => $params{callbacks}, original_values => $caller_params, check_only => $check_only);
                if (!$ok)
                {
                    $on_fail->("$called $msg");
                    return;
                }
                # optional fields become required if the field they
                # depend on is present
                if ($signature_info->{map}{$param}->{depends})
                {
                    push(@required,@{$signature_info->{map}{$param}->{depends}});
                }
            }
            print_debug("idx: $idx, extra_cutoff: $extra_cutoff");
            if (($extra_cutoff) &&
                ($idx + 1 == $extra_cutoff) &&
                ((scalar @{$params{params}} % 2) != 0))
            {
                $on_fail->("$called Uneven named parameters found near '$params{params}[0]', so a hash cannot be built");
                return;
            }

            if ($param_style eq $MIXED)
            {
                    push(@return_list, $param, $value);
            }
            else
            {
                if (defined($signature_info->{map}{$param}->{idx}))
                {
                    $return_list[$signature_info->{map}{$param}->{idx}] = $value;
                }
                else
                {
                    # stuff extra params onto end of return_list; caller will have
                    # to figure out what each value is ... caller should probably use
                    # return_hash
                    push(@return_list, $value);
                }
            }
            $return_hash{$param} = $value;
        }

        print_debug("idx: $idx, extra_cutoff: $extra_cutoff");
    }

    # confirm we have all of the required, named parameters
    # (positional parameters are handled in POSITIONAL section above)
    foreach $param (@required)
    {
        if (!defined($return_hash{$param}))
        {
            #$on_fail->("$called Required parameter '$param' is not present");
            print_debug("Required parameter '$param' is not present");
            ($value, $ok, $msg) = $self->validate_arg(value => undef, spec => $signature_info->{map}{$param}, info => $signature_info, set_values => \%return_hash, callbacks => $params{callbacks}, original_values => $caller_params, check_only => $check_only);
            if (!$ok)
            {
                $on_fail->("$called $msg");
                return;
            }
            if ($param_style eq $MIXED)
            {
                    push(@return_list, $param, $value);
            }
            else
            {
                $return_list[$signature_info->{map}{$param}->{idx}] = $value;
            }
            $return_hash{$param} = $value;
        }
    }

    print_debug("return_list:", Dumper(\@return_list));
    print_debug("return_hash:", Dumper(\%return_hash));
    print_debug("done: validate");
    (wantarray ? @return_list : \%return_hash); 
}

sub validate_arg
{
    my $self = (ref($_[0])) ? shift @_ : class_default();
    my %params = @_;
    my $ok = 1;
    my $msg = undef;
    my $param_name;
    # get values out of %params so we don't do
    # unnecessary hash lookups
    my $value = $params{value};
    my $spec = $params{spec};
    my $set_values = $params{set_values};
    my $original_values = $params{original_values};
    my $callbacks = $params{callbacks};
    my $check_only = $params{check_only};
    my $cbs;

    print_debug("validate_arg:", %params);

    # if no value, assign default value
    if (!defined($value) && !$check_only)
    {
        if ($spec->{default_type} == $ASSIGN_LITERAL)
        {
            $value = $spec->{default};
        }
        elsif ($spec->{default_type} == $ASSIGN_SUB)
        {
            $value = $spec->{default}->();
        }
        elsif ($spec->{default_type} == $ASSIGN_PARAM)
        {
            $param_name = $spec->{default};
            $value = $set_values->{$param_name};
        }
    }

    # check value type
    $ok = $self->type_check($spec->{type}, $value);
    if (!$ok)
    {
        $msg = "$spec->{name} assigned invalid type of value ($value), expected $spec->{type}";
    }
    elsif (!$check_only) 
    {
        # run callbacks 
        $cbs = $callbacks->{$spec->{name}};
        print_debug("Callbacks for $spec->{name}:", Dumper($cbs));
        foreach my $c (keys(%{$cbs}))
        {
            print_debug("Callback for $spec->{name}:", Dumper($c));
            $ok = $cbs->{$c}->($value, $original_values, $set_values);
            if (!$ok)
            {
                $msg = "Failed callback '$c' for '$spec->{name}'";
                last;
            }
        }
    }
    return ($value, $ok, $msg);
}

sub _get_signature_info
{
    my $self = (ref($_[0])) ? shift @_ : class_default();
    my $cache_key;
    my %params;
    my $signature_info;
    my $ok = 1;
    my $msg = "";
    my $on_fail = $self->{on_fail};
    my $called = $self->{called};

    if (scalar @_ == 1)
    {
        $params{signature} = shift;
    }
    else
    {
        %params = @_;
    }

    if (!defined($params{signature}))
    {
        $ok = 0;
        $msg = "Cannot get signature information because signature is missing";
    }
    else
    {
        $cache_key = join("$FIELD_SEPARATOR", @{$params{signature}});

        if (defined($self->{cache}{$cache_key}))
        {
            print_debug("Use cache key: $cache_key");
            $signature_info = $self->{cache}{$cache_key};
        }
        else
        {
            print_debug("Make signature_info for new cache key: $cache_key");
            # generate spec for each argument defined in signature
            ($signature_info, $ok, $msg) = $self->_build_signature_info($params{signature});
            if ($ok)
            {
                $self->{cache}{$cache_key} = $signature_info;
            }
        }
    }
    return (wantarray ? ($signature_info, $ok, $msg) : $signature_info); 
}

sub _get_param_style
{
    my $self = (ref($_[0])) ? shift @_ : class_default();
    my %params;
    my $extra_cutoff;
    my $positional_cutoff;
    my $param_style;
    my $signature_spec;
    my $signature_info;
    my $signature_param_count;
    my $fuzzy;
    my $param_count;
    my $required_count;
    my $normalize_keys;
    my $on_fail;
    my $ok = 1;
    my $msg = "";

    print_debug("get_param_style");

    if (scalar @_ == 5)
    {
        $params{params} = shift;
        $params{param_style} = shift;
        $params{fuzzy} = shift;
        $params{normalize_keys} = shift;
        $params{signature_info} = shift;
    }
    else
    {
        %params = @_;
    }
    print_debug(Dumper(\%params));
    $fuzzy = $params{fuzzy};
    $normalize_keys = $params{normalize_keys};

    if (!(defined($params{params})) ||
        !(defined($params{signature_info})))
    {
        $ok = 0;
        $msg = "Cannot determine param_style because parameters are missing";
    }
    else
    {
        $param_count = scalar @{$params{params}};

        $signature_info = $params{signature_info};
        $signature_spec = $signature_info->{signature_spec};
        $signature_param_count = scalar @{$signature_spec};
        $positional_cutoff = $signature_info->{positional_cutoff};
        $extra_cutoff = $signature_info->{extra_cutoff};
        $required_count = scalar (keys %{$signature_info->{required}});

        print_debug("fuzzy = $fuzzy");
        print_debug("param_count = $param_count");
        print_debug("required_count = $required_count");
        print_debug("extra_cutoff = $extra_cutoff");
        print_debug("positional_cutoff = $positional_cutoff");
        print_debug("signature_param_count = $signature_param_count");

        # param_style actually passed in overrides all other considerations
        if ($params{param_style})
        {
            print_debug("param_style passed in with params");
            # allow for regex that matches a key:
            # pos, name, mix, p, n, m, etc.
            ($param_style) = grep { $_ =~ /$param_style/ } [$POSITIONAL, $NAMED, $MIXED];
        }
        # use info derived from signature
        elsif (defined($extra_cutoff) && $extra_cutoff == 0) { $param_style = $POSITIONAL; }
        elsif (defined($positional_cutoff) && $positional_cutoff == $signature_param_count) { $param_style = $POSITIONAL; }
        elsif (defined($positional_cutoff) && $positional_cutoff > 0) { $param_style = $MIXED; }
        elsif (defined($positional_cutoff) && $positional_cutoff == 0) { $param_style = $NAMED; }
        # use "fuzzy logic" to guess
        elsif ($fuzzy && ($signature_param_count == $param_count))
        {
            print_debug("sig_param_count == param_count");
            # every parameter is present and we
            # have no other clues to indicate which parameter style
            # to use
            $param_style = $POSITIONAL;
        }
        elsif ($fuzzy && ($signature_param_count > 1) && ($param_count == 1) && (ref($params{params}[0]) eq "HASH"))
        {
            print_debug("1 HASH param");
            # we should have more than one parameter, but we've
            # been given just one hash and no other clues; going
            # to assume sub was called using named parameters stuffed
            # into an anonymous hash ...
            #
            # Example:
            #          call_sub({ field => value, f2 => val2 });
            #  - vs -
            #          call_sub(value, val2);
            # in call_sub: $signature->validate(\@, $spec);
            # note that call_sub simply passes \@_ to validate() in both cases
            $param_style = $NAMED;
        }
        elsif ($fuzzy &&
                ($required_count) &&
                ($param_count >= ($required_count * 2)) &&
                ($self->_check_for_named_param($params{params}, $signature_info->{required}, $normalize_keys)))
        {
            print_debug("all $required_count required param present");
            # if we have all of the required fields in the proper
            # index position of the array, it looks like a hash
            # or valid argument list we can work with
            $param_style = $NAMED;
        }
        else
        {
            # nothing explicit about the param style to use,
            # fuzzy is disabled or failed to figure it out,
            # so use the default
            (defined($self->{param_style})) ? print_debug("using \$self->{param_style}") : print_debug("using \$class_default->{param_style}");
            $param_style = (defined($self->{param_style})) ? $self->{param_style} : $class_default->{param_style};
        }
    }

    print_debug("Param style: $param_style");

    return (wantarray ? ($param_style, $ok, $msg) : $param_style); 
}

sub _check_for_named_param
{
    my $self = shift;
    my $params = shift;
    my $req = shift;
    my $normalize = shift;
    my $passed = 1;
    my $param;
    my $match_count = 0;
    my $i = 0;
    my $pass_count = scalar (keys %$req);

    print_debug("check_for_named_param:", Dumper($params), Dumper($req), Dumper($normalize));

    for ($i = 0, $match_count = 0;
        $i < scalar @{$params} && $match_count < $pass_count;
        $i += 2, $match_count += $passed)
    {
        $param = (defined($normalize)) ? $normalize->($params->[$i]) : $params->[$i];
        if (!defined($req->{$param}))
        {
            $passed = 0;
            next;
        }
        $passed = 1;
    }
    print_debug("match=$match_count, pass=$pass_count\n");
    return $match_count == $pass_count;
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

Params::Signature - support for parameter validation based on a subroutine signature, including type declaration, default values, optional parameters, and more

=head1 VERSION

Version 0.01

=cut

our $VERSION = '0.01';


=head1 SYNOPSIS

    use Params::Signature;

    # 'param_style' defaults to positional, but setting explicitly anyway
    my $signature = new Params::Signature(param_style => 'positional');

    # all positional params
    sub_one(1,"hi")

    sub sub_one
    {
        my $params_hashref = $signature->validate(\@_, ["Int one", "Str two"]);
        # - or -
        my @params_array = $signature->validate(\@_, ["Int one", "Str two"]);

        # $params_hashref = { one => 1, two => 'hi' };
        # @params_array = [ 1, 'hi' ];
        ...
    }

    sub_two(1,"hi",{option_one => 3});

    sub sub_two
    {
        # call validate() with named parameters
        my $params = $signature->validate(params => \@_, signature => ["Int one", "Str two", "HashRef options"]);
        # get a hash ref back: { one => 1, two => 'hi', options => {option_one => 3} }
        # - or -
        # my @params_array = $signature->validate(params => \@_, signature => ["Int one", "Str two", "HashRef options"]);
        # get an array: [ 1, 'hi', {option_one => 3} ]
        ...
    }

    # all subroutines use named parameters
    my $named_signature = new Params::Signature(param_style => 'named');
    named_sub_one(one => 1, two => 'hi', options => {other => 3});
    named_sub_one({one => 1, two => 'hi', options => {other => 3}});
    named_sub_one(1, 'hi', {other => 3}); # ERROR!
    sub named_sub_one
    {
        my $params = $signature->validate(params => \@_, signature => ["Int one", "Str two", "HashRef options"]);
        # - or -
        # my @params_array = $signature->validate(params => \@_, signature => ["Int one", "Str two", "HashRef options"]);
        ...
    }

    # subroutines can mix parameter types (positional and named)
    my $signature = new Params::Signature();
    mixed_sub_one(1, 'hi', {other => 3});
    mixed_sub_one(1, 'hi', other => 3);
    mixed_sub_one(1, 'hi', other, 3);
    sub mixed_sub_one
    {
        # parameters are assumed to be positional,
        # named parameters have names that start with ":" (like :other)
        my $params = $signature->validate(params => \@_, signature => ["Int one", "Str two", "Int :other"]);
        # - or -
        # use special 'named:' pseudo-param to separate positional from named params
        # my @params_array = $signature->validate(params => \@_, signature => ["Int one", "Str two", "named:", "Int other"]);
        ...
    }

    # enable 'fuzzy validation' to allow caller to use positional or
    # named arguments without changing code in called subroutine;
    # 'fuzzy validation' lets $signature->validate() determine the
    # calling style and process parameters accordingly
    my $fuzzy_signature = new Params::Signature(fuzzy => 1);

    # use positional arguments
    call_fuzzy_style(1, 2)

    # use named arguments
    call_fuzzy_style(one => 1, two => "hi")

    # use named arguments in a hash 
    call_fuzzy_style({one => 1, two => "hi"})

    sub call_fuzzy_style
    {
        # don't need to change signature regardless of how we're called
        my $params = $fuzzy_signature->validate(\@_, ["Int one", "Str two"]);
        
        ...
    }


=head1 DESCRIPTION

In its simplest form, you simply call Params::Signature's validate method with your parameters and a signature specification:

    $params = $signature->validate(\@_, ["Int one", "Str two = 'A default value'", "Undef|Str three?]);

The signature is a list of parameter definitions.  A basic parameter definition consists of a type constraint and the name of the parameter.  The built in types were shamelessly stolen from Moose and re-implemented.  In addition, it's simple to add your own types using the L</register_type> method.  A parameter's type constraint can include multiple types (e.g., Undef or Str).  Parameters are required by default.  A default value may be assigned to a required parameter.  Parameters may be flagged as optional using the optional flag (trailing question mark).

More advanced scenarios are also supported.  Per-parameter callbacks can be used for advanced parameter validation.  Parameter aliases can be used to call a parameter by different names.  In some cases, instead of using aliases, it may make more sense to use a callback to normalize parameter names.

    $params = $signature->validate(
        params => \@_, 
        signature => ["Int one|uno", "Str two|dos = 'A default value'"], 
        param_style => "named",
        normalize_keys => sub { $_[0] =~ s/^-//; lc $_[0] },
        callbacks => {
            one => {
                "is less than 2" => sub { $_[0] < 2 },
                "is greater than 0" => sub { $_[0] > 0 }
                }
            }
        );

=cut

=head2 Parameter Style

Subroutines in perl are commonly called using different parameter styles - positional, named or a mixture of both.  Knowing the parameter style is necessary in order to properly interpret the values that are being validated.  

There are multiple ways to select the parameter style.  An explicit parameter style may be specified when the validate method is called.  The signature itself may be defined in such a way as to identify the parameter style.  A default parameter style may be specified when a Params::Signature object is generated.  If it is not specified, the global default is positional.  In other words, there's more than one way to do it.

The validate method first looks to see if an explicit style has been passed in to the validate method. Then the signature is examined.  Next, if 'fuzzy' is enabled, a number of checks are performed in an attempt to determine which style was actually used.  The guessing logic attempts to be conservative and should work as expected in most cases (of course, this assumes you have the right expectations in most cases).  The Params::Signature object is then consulted.  Lastly, if all else fails, the global default (positional) is used.

Each subroutine can potentially use a different parameter style.  In practice, it's probably best to set the style in the Params::Signature object and then use the same style throughout a module or application, but that is not a requirement.

    # specify a default for the object
    my $signature = new Params::Signature(param_style => "positional");

    positional_only(1, 'hi', 3);
    use_named_only(one => 1, two => 'hi', three => 3);
    also_use_named_only({one => 1, two => 'hi', three => 3});
    use_mixed_style(1, 2, 3, {four => 4, five => 5});

    sub positional_only
    {
        # signature indicates that everything before 'named:' pseudo-parameter is positional only
        my $params = $signature->validate(\@_, ['Int one', 'Str two', 'Num three', 'named:']);
    }

    sub use_named_only
    {
        # explicitly indicate that only named parameters are accepted
        my $params = $signature->validate(param_style => "named", params => \@_, signature => ['Int one', 'Str two', 'Num three']);
    }

    sub also_use_named_only
    {
        # signature indicates that only named parameters are accepted using 'named:' pseudo-parameter
        # use this method if you want to pass @_ and the signature as positional arguments to validate
        my $params = $signature->validate(\@_, ['named:', 'Int one', 'Str two', 'Num three']);
    }

    sub use_mixed_style
    {
        # signature indicates that 'one' is positional only and 'two' is named only
        my $params = $signature->validate(\@_, ['Int one', 'Int two', "Int three", "named:", "Num four", "Num five"]);
        #  - or -
        # indicate that some of the parameters are named using perl6-ish syntax
        # parameters are positional by default, those that begin with :$ are named only
        # the $ and : are stripped from the keys used in $params
        # my $params = $signature->validate(params => \@_, signature => ['Int $one', 'Int $two', "Int $three", "Num :$four", "Num :$five"]);
    }

=cut

=head2 Parameter Signature

The signature itself is a list of parameter definition strings.  Each parameter is defined in the order in which arguments should be passed to a subroutine, if a positional parameter style is used.  The signature may contain pseudo-parameters.  Parameters appearing after the C<named:> pseudo-parameter are always named when the subroutine is called.  The extra (C<...>) pseudo-parameter may be used to indicate that extra parameters are allowed.  Parameter names which begin with a C<:> or C<:$> are named parameters, those that begin with nothing or just a C<$> are positional.

    ['Int one', 'Int two', "Int three", "named:", "Num four", "Num five", "..."]
    \_______________________________/   \_____/   \____________________/  \__/
                   /                       /                /              /
    positional ----                       /                /              /
    pseudo-parameter ---------------------                /              /
    named parameters -------------------------------------              /
    extra parameters indicator -----------------------------------------

    # using perl6 style parameter names
    ['Int $one', 'Int $two', "Int $three", "Num :$four", "Num :$five", "..."]
    \__________________________________/    \____________________/      \__/
                      /                              /                   /
    positional only --                              /                   /
    named parameters -------------------------------                   /
    extra parameters indicator ----------------------------------------

=cut


=head1 PARAMETER DEFINITION

A parameter is made up of a type constraint, a name (and aliases), an optional/required flag, and an assignment indicator with a value.

                       +-- type constraint
                       |            +-- parameter name and aliases
                       |            |       +-- optional/required flag
                       |            |       |   +-- indicator (= or <= or <<)
                       |            |       |   |     +-- indicator value (literal, dependents or subroutine)
                       |            |       |   |     |
                     ______      _________  _   _    ______
                    /      \    /         \/ \ / \  /      \
                    Str|Int     username|id?    =   'not set'
                     /   /       /      /  /        /
    parameter type --   /       /      /  /        /
  alternate param type -       /      /  /        /
         parameter name  ------      /  /        /
     parameter alias    -------------  /        /
     optional(?) or required(!) -------        /
    default value -----------------------------

=head2 Type Constraint

The value assigned to a parameter must match at least one of the type constraints assigned to the parameter.  Multiple type constraints are separated by an or bar (pipe symbol).  New types can be added to a Params::Signature object or to the global default using L</register_type>, L</register_class> and L</register_role>.

The default types are:

        Any
          Item
              Bool
              Undef
              Defined
                  Value
                      Str
                          Num
                              Int
                          ClassName
                  Ref
                      CodeRef
                      RegexpRef
                      GlobRef
                      FileHandle
                      Object

=cut

=head2 Parameter Name and Aliases

A parameter is assigned a name which is used as a key when the validate method returns a hash reference.  One or more optional aliases can also be defined for a parameter.  When aliases are used, the first value is considered the name and all subsequent values are aliases.  The name is used as a key when the L</validate> method returns a hash reference.

The parameter name is defined without any leading sigil character ($@%).  However, the perl 6-ish naming style of preceding every positional value with a "$" and named parameters with ":$" is supported.  For the sake of brevity, a parameter name which begins with just a colon (e.g., "Int :named_param") is also considered a named parameter.  This is where similarity with perl 6 parameter signatures begins and ends.  The full range of perl 6-style signature definitions is currently not supported.  Furthermore, any leading sigils (:$) are not part of the keys in the hash returned by the L</validate> method.  They are supported because they look "modern" and looking modern gives some people a warm, fuzzy feeling inside.

=cut

=head2 Optional vs Required

By default, a parameter is considered required.  The optional flag (a question mark -- ?) can be appended to the name (or last alias) to define a parameter as optional.  The exclamation point (!) can be used to explicitly declare a value as required.

=cut

=head2 Indicator and Indicator Values

A parameter can use one of 3 indicators.  The C<=> (equal sign) is used to assign a default value to a parameter.  The value may be a literal value or the value returned by a subroutine.  The C<<=> (back arrow) is used to assign the value of another "from" parameter to a parameter that is not set.  The "from" parameter must appear in the signature before it is used.  The C<<<> (double arrow) is used to define a parent field's dependents.

=head2 Default Value

If a required parameter is not set, a default value may be assigned to it.  The default value may be a literal, the value of another parameter or the value returned by a subroutine.  Default values are not be assigned to optional values.  However, there is one exception to this rule.  A default value may be assigned to an optional parameter which becomes required when a field it depends on is set (see L</Dependents>).  The "<=" (back arrow) is used to assign the value of another parameter as the default value of an unset parameter.

    # Assign a literal value as the default
    ["Str login = 'not set'"]

    # Assign the value of another parameter as the default value
    # If 'bar' is not set, it defaults to the value already assigned to 'foo'
    ["Str foo", "Str bar <= foo"]

    ["Str foo = { get_a_foo() }"]

=cut

=head2 Dependents

A list of fields may be declared as being dependent on the presence of another optional parameter.  The "<<" indicator stands for 'dependents'.  The indicator is always followed by a list of one or more dependent parameter names.  The default values defined for the dependents are only assigned if the parent field is already set.

    # if 'foo' is set, the optional parameters 'bar' and 'baz' become required parameters
    # if foo is not set, all 3 parameters are optional and therefore not set
    ["Str foo? << [bar, baz]", "Str bar? = 'default bar'", "Str baz? = { get_next_baz() }"]


=cut

=head2 Callbacks

At times, a type constraint is not enough to validate a parameter.  Callbacks can be defined for each parameter.  Each callback must return a true value, otherwise, the parameter is rejected and the 'on_fail' subroutine is executed.

    $params = $signature->validate(
        params => \@_, 
        signature => ["Int one"], 
        param_style => "positional",
        callbacks => {
            one => {
                "is less than 2" => sub { $_[0] < 2 },
                "is greater than 0" => sub { $_[0] > 0 }
                }
            }
        );

=cut

=head2 Named and Extra Pseudo-Parameters

The 'C<named:>' pseudo-parameter is used to designate that all subsequent parameters in the signature must always appear as named parameters in calls to the subroutine.  Placing C<named:> as the first item in a signature indicates that all parameters must always be named, hence the subroutine always uses the 'named' parameter style.  Making C<named:> the last item in the signature indicates that all preceding parameters are always positional, hence the 'positional' style should always be used when calling the subroutine.  If C<named:> appears in the middle of a signature, it separates positional from named parameters.

    # positional only signature
    ["Int one", "Int two", "named:"] 

    # named only signature
    ["named:", "Int one", "Int two"]  

    # mixed signature
    ["Int one", "named:", "Int two"]  

The extra ('C<...>') pseudo-parameter is used to indicate that extra parameters are allowed.  Extra parameters are not validated, they are simply appended to the list returned by the L</validate> method.  If a signature ends with named parameters, then all extra parameters must also be named.  A 'best effort' attempt is made to ensure the extra parameters are named.  If a signature ends with positional parameters, all extra parameters are treated as positional as well.

    my $param = $signature->(\@_, ["Int one", "Int two", "..."]);

If a signature starts with a "...", all validation is disabled since all parameters are considered "extra parameters".

=cut


=head1 METHODS

All functionality is accessed via methods.  You can use C<Params::Signature->method()> or construct a module or application-specific Params::Signature object.  Class methods may be used to validate parameters using global settings.  By default, parameters are positional, fuzzy logic is disabled, and Carp::confess is used to report failures.  Class methods work just like object methods.

    Params::Signature->register_type(name => "EvenInt", parent => "Int", where => sub { $_[0] % 2 == 0 }, inline_as => sub { '$_[0] % 2 == 0'});
    Params::Signature->validate(\@_, ["Int one", "EvenInt two", "Str three"]);

    #  - or -

    my $signature = new Params::Signature();
    $signature->register_type(name => "EvenInt", parent => "Int", where => sub { $_[0] % 2 == 0 }, inline_as => sub { '$_[0] % 2 == 0'});
    $signature->validate(\@_, ["Int one", "EvenInt two", "Str three"]);


=head2 new

    new Params::Signature(
            param_style => "positional",
            fuzzy => 1,
            on_fail => sub { oops($_[0]) },
            normalize_keys => sub { lc $_[0] },
            called => "My::Module"
        );

B<param_style>: Parameter style is one of 'positional', 'named' or 'mixed'.  The actual signature passed to L</"validate"> may override the default set in the new signature object.  The default value in the object is used when the actual subroutine signature lacks sufficient information to determine the parameter style.

B<fuzzy>: Allow the L</"validate"> method method to use 'fuzzy logic' to determine the parameter passing style used to invoke the caller.  Enable this if you want to be able to call a subroutine with either positional parameters or key/value pairs that match the subroutine signature.  When using named parameters, all required field names must be present in @_ or simply pass in all parameters as a hash.

    my $fuzzy_signature = new Params::Signature(fuzzy => 1);

    ...

    # ok: all parameters defined in signature are present - positional
    foo(1, 2, 3); 

    # ok: both required parameter's names are present and at correct index
    foo(one => 1, two => 2);

    # better: hash makes it clear that 'named' style is in use
    foo({one => 1, two => 2});

    # DANGER: this looks like 2 positional parameters! 
    foo(one => 1);

    sub foo
    {
        my $param = $fuzzy_signature->validate(\@_, ["Int one", "Int two", "Int three?"]);
        ...
    }

If fuzzy is enabled, it's safest to pass named values in one anonymous hash or positional values at the appropriate index position.

B<on_fail>:  Set the subroutine to call if an error is encountered.  Carp::confess is called by default.

B<normalize_keys>: A subroutine to normalize named parameters passed in to caller.

    my $signature = new Params::Signature(param_style => "named", normalize_keys => sub { $_[0] =~ s/^-//; lc $_[0] });

    sub foo
    {
        my $params = $signature->validate(params => \@_, signature => ["Int one"]);
        ...
    }
    foo(-one => 1);
    foo(-ONE => 1);
    foo(one => 1);

B<called>: String inserted at the beginning of each failure message.  Your module or application name are good candidates for this value.

=cut

=head2 validate

Validate the parameters passed to a subroutine using a subroutine signature.

    # use class method
    # my $params = Params::Signature->validate(\@_, ["Int one", "Int two", "Int :$named_param", "Int :$opt_named", "..."]);

    # use localized object
    my $params = $signature->validate(\@_, ["Int one", "Int two", "Int :$named_param", "Int :$opt_named", "..."]);

    my $params = $signature->validate(
                    params => \@_,
                    signature => ["Int one", "Int two", 'named:', "Int named_param", "Int opt_named", "..."],
                    param_style => "mix",
                    normalize_keys => sub { lc $_[0] },
                    fuzzy => 1,
                    called => "YourModule",
                    on_fail => \&catch_validation_error
                    callbacks => {
                        one => {
                            "equals one" => { $_[0] == 1}
                            }
                        },
                );

You can also validate the parameters passed to a method.

    my $self = shift;
    my $params = $signature->validate(\@_, ["Int one", "Int two", "Int :named_param", "Int :opt_named", "..."]);

B<params>: A reference to an array of parameters passed to the calling subroutine.  This array is left alone and may be used after the call to validate.

B<signature>: The actual subroutine signature is an array with each element representing one parameter.  Positional parameters are expected to be passed in in the same order as they appear in the signature.

B<param_style>: Explicitly set the parameter style used to validate parameters.

B<normalize_keys>: A reference to a subroutine.  For named parameters, alter the keys used to call the calling subroutine to match the parameter names used in the signature.  The names in the signature are not passed to this subroutine.

B<fuzzy>: Enable 'fuzzy logic' used to determine what type of parameter style was used.  This overrides the default in the Params::Signature object.

B<called>: A string included in any error messages produced.

B<on_fail>: Override Carp::confess as the subroutine that gets called when a failure occurs.

B<callbacks>: A hash for fine-grained testing of values which goes beyond type checking.  The hash keys match the parameter names in the signature.  The value of each key is a hash of test names and subroutine references.  This allows per-parameter validation callback routines.  The callback routine receives 3 parameters - the parameter value, the original values passed to the validate method, a hash containing a list of values that have already been validated.  The last hash is ultimately returned to the caller, if validate is called in scalar context.

B<Return Value>:

In scalar context, the method returns a hash reference with key/value pairs for each parameter that has a value.  In list context, this method returns a list of parameter values in the order of appearance in the signature.  If extra parameters are passed in (and allowed), they are appended to the list in the order of appearance in 'params' (which is usually @_).  If a mixed parameter style was used, the list contains positional parameters in the order they appear in the signature.  Named parameters appear as a key followed by a value in the list.  If you are using a mixed parameter style, it may be easier to call the validate method in scalar context and use keys to access all parameters.  That said, a (somewhat) sane result is returned.

    mixed_foo(1, undef, three => 3, four => 4);
    sub mixed_foo
    {
        # get hash in scalar context (and use a perl 6-ish signature)
        my $params = $signature->validate(\_@, ["Int $one", "Int $two = 2", "Int :$three", "..."]);
        # $params = { one => 1, two => 2, three => 3, four => 4 }

        # get a list in list context (signature happens to use the "native" signature style)
        my @params = $signature->validate(\_@, ["Int one", "Int two = 2", "named:", "Int three", "..."]);
        # @params = [ 1, 2, three, 3, four, 4]
    }

    bar(1,undef,3)
    sub bar
    {
        # get hash in scalar context (and use a perl 6-ish signature)
        my $params = $signature->validate(\_@, ["Int $one", "Int $two", "Int $three"]);
        # $params = { one => 1, two => undef, three => 3}

        # get a list in list context (I happen to use the "native" signature style)
        my @params = $signature->validate(\_@, ["Int one", "Int two", "Int three"]);
        # @params = [ 1, undef, 3 ]
    }

=cut

=head2 register_type

Register a new type which can be used in a signature.

    $signature->register_type(name => "EvenInt", parent => "Int", where => sub { $_[0] % 2 == 0 }, inline_as => sub { '$_[0] % 2 == 0'});
    ...
    my $param = $signature->validate(\@_, ["EvenInt even_num"]);

B<name>: the name of the new type

B<parent>: the name of the parent type (if this type inherits the characteristics of an existing type)

B<where>: subroutine which performs the type validation test

B<inline_as>: subroutine which returns a string which can be inlined to test a value. You can assume it will be surrounded by parentheses when it is executed.

The default types are found in the L</Type Constraint> section.

=cut

=head2 register_class

Register a class as a type which can be used in a signature.  The isa method is called on a value to determine if it is of the right class type.

    $signature->register_class(name => "MyClass");
    ...
    use MyClass;
    my $param = $signature->validate(\@_, ["MyClass even_num"]);

=cut

=head2 register_role

Register a role as a type which can be used in a signature.  A value which DOES the role will pass the type constraint test.  The DOES method is used to determine if a value does the named role.

    $signature->register_role(name => "DoesX");
    ...
    my $param = $signature->validate(\@_, ["DoesX param_one"]);

=cut

=head2 type_check

This method is used to confirm a value meets a type constraint.  The type constraint can be made up of multiple types.  This method may be useful when writing C<where> and C<inline_as> tests for evaluating complex, custom types.

    my $is_ok = $signature->type_check("Int|Num|Undef", $value)

B<type>: the type constraint, which can be multiple types separated by an or bar (pipe symbol)

B<value>: the value to be checked

B<Return Value>:

Returns 1 if the value matches at least one type in the type constraint or 0 otherwise.


=cut

=head1 PERFORMANCE

The Params::Signature object caches the parsed form of each signature it validates.  Re-using the same object to validate subroutine parameters eliminates the need to parse the signature every time.  Using a singleton per module or application is recommended for reducing the amount of time it takes to validate parameters.


=head1 LIMITATIONS AND CAVEATS

The current implementation requires that you either define your own Params::Signature object or use the class as an object (Params::Signature->validate).  It's recommended that your module or application define a singleton to use to validate parameters.  Using a separate object per thread should be safe, though this has not been tested. 

Container-style type constraints are currently not supported directly.  For example, you cannot define a parameter as "ArrayRef[Int]" and have the validate method confirm that the parameter is an array of integers.  A custom type with an appropriate C<where> and C<inline_as> parameter could be used to accomplish this.  That's left as an exercise to the reader.

There is no XS version of this module at this time.  It's pure perl.  Perhaps that's a feature rather than a limitation?

This documentation might not be very clear, even though I know exactly what I meant to say at the time I tried to say it.

=cut

=head1 AUTHOR

Sandor Patocs, C<< <perl at patocspack.com> >>

=head1 BUGS

Please report any bugs or feature requests to C<bug-params-signature at rt.cpan.org>, or through
the web interface at L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=Params-Signature>.  I will be notified, and then you'll
automatically be notified of progress on your bug as I make changes.


=head1 SUPPORT

You can find documentation for this module with the perldoc command.

    perldoc Params::Signature


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

Params::Validate, MooseX::Method::Signatures and Method::Signatures all served as inspiration for this module.

=head1 SEE ALSO

L<Params::Validate>, L<MooseX::Method::Signatures>, L<Method::Signatures>, L<Perl6::Signature>


=head1 LICENSE AND COPYRIGHT

Copyright 2013 Sandor Patocs.

This program is distributed under the terms of the Artisitic License (2.0)


=cut

1; # End of Params::Signature