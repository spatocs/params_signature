#!perl 
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

use Data::Dumper;
use Try::Tiny;
use Params::Signature qw(:all);

my $test_count = 0;

our $failed;
our $failed_msg = "";

sub catch_failed
{
    $failed_msg = shift;
    $failed     = 1;
}


Main:
{
    my $test_sub;
    my $test_sub_name;
    my $signature;

    diag("Test custom type constraint coercion via methods, Perl $], $^X");

    foreach $test_sub_name (sort grep /^test_/, keys(%main::))
    {
        $failed     = 0;
        $failed_msg = "";
        #diag($test_sub_name);

        $signature = new Params::Signature(param_style => "named", on_fail => \&catch_failed, register_builtins => 1, called => '');
        # clear out cruft left behind by previous test
        Params::Signature->_change_class_default($signature);
        # NOTE: _change_class_default should not be used in applications!

        # get around "strict refs" when calling subroutine
        $test_sub = \&{"$test_sub_name"};
        &$test_sub($signature, $test_sub_name);
    }
}

# use cases
sub test_coerce_hash_ref_to_params_signature
{
    my $signature = shift;
    my $name = shift;
    my $answer;
    my $info = {};

    $signature->register_class("Params::Signature");
    $signature->register_coerce("HashRef", "Params::Signature", sub { new Params::Signature(%{$_[0]})});

    $answer = $signature->coerce(to => "Params::Signature",
                    from => { param_style => 'named', register_builtins => 0, called => 'testXYZ'},
                    info => $info
        );
    $failed_msg = $info->{msg};

    ok($answer && (ref($answer) eq "Params::Signature") && ($answer->{called} eq 'testXYZ') && $info->{msg} eq "ok", "$name: $failed_msg");
}

sub test_coerce_hash_ref_to_params_signature_many_types
{
    my $signature = shift;
    my $name = shift;
    my $answer;
    my $info = {};

    $signature->register_class("Params::Signature");
    $signature->register_coerce("HashRef", "Params::Signature", sub { new Params::Signature(%{$_[0]})});

    $answer = $signature->coerce(to => "Str|Int|Params::Signature",
                    from => { param_style => 'named', register_builtins => 0, called => 'testXYZ'},
                    info => $info
        );
    $failed_msg = $info->{msg};

    ok($answer && (ref($answer) eq "Params::Signature") && ($answer->{called} eq 'testXYZ') && $info->{msg} eq "ok", "$name: $failed_msg");
}

sub test_coerce_already_to_type
{
    my $signature = shift;
    my $name = shift;
    my $answer;
    my $info = {};

    $signature->register_class("Params::Signature");
    $signature->register_coerce("HashRef", "Params::Signature", sub { new Params::Signature(%{$_[0]})});

    $answer = $signature->coerce(to => "Params::Signature",
                    from => $signature,
                    info => $info
        );

    $failed_msg = $info->{msg};
    ok($answer == $signature && (ref($answer) eq "Params::Signature") && $info->{msg} eq "ok", "$name: $failed_msg");
}

sub test_coerce_already_one_of_to_type
{
    my $signature = shift;
    my $name = shift;
    my $answer;
    my $info = {};

    $signature->register_class("Params::Signature");
    $signature->register_coerce("HashRef", "Params::Signature", sub { new Params::Signature(%{$_[0]})});

    $answer = $signature->coerce(to => "Str|Num|Class|Params::Signature",
                    from => $signature,
                    info => $info
        );

    $failed_msg = $info->{msg};
    ok($answer == $signature && (ref($answer) eq "Params::Signature") && $info->{msg} eq "ok", "$name: $failed_msg");
}

sub test_coerce_invalid_ref_to_params_signature
{
    my $signature = shift;
    my $name = shift;
    my $answer;
    my $info = {};

    $signature->register_class("Params::Signature");
    $signature->register_coerce("HashRef", "Params::Signature", sub { new Params::Signature(%{$_[0]})});

    $answer = $signature->coerce(to => "Params::Signature",
                    from => [ param_style => 'named', register_builtins => 0, called => 'testXYZ'],
                    info => $info
        );
    $failed_msg = $info->{msg};

    ok(!$answer && (ref($answer) ne "Params::Signature"), "$name: $failed_msg");
}


$test_count += scalar grep /^test_/, keys(%main::);
plan tests => $test_count;
