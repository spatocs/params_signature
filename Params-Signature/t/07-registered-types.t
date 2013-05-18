#!perl 
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

use Data::Dumper;
use Try::Tiny;
use Params::Signature;

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
    my $signature = new Params::Signature(param_style => "named", on_fail => \&catch_failed, register_builtins => 1);

    diag("Test custom type constraint registration and use, Perl $], $^X");

    foreach $test_sub_name (sort grep /^test_/, keys(%main::))
    {
        $failed     = 0;
        $failed_msg = "";
        #diag($test_sub_name);

        $signature = new Params::Signature(param_style => "named", on_fail => \&catch_failed, register_builtins => 1);

        # get around "strict refs" when calling subroutine
        $test_sub = \&{"$test_sub_name"};
        &$test_sub($signature, $test_sub_name);
    }
}

# use cases
sub test_register_class
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_class("Params::Signature");

    $answer = $signature->validate(
        params    => [sig => $signature],
        signature => ["Params::Signature sig"],
        );

    ok(!$failed && $answer->{sig} == $signature, "$name: $failed_msg");
}

sub test_register_role
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_role("Params::Signature");

    $answer = $signature->validate(
        params    => [sig => $signature],
        signature => ["Params::Signature sig"],
        );

    ok(!$failed && $answer->{sig} == $signature, "$name: $failed_msg");
}

sub test_register_can
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_can("CanValidate", "validate");

    $answer = $signature->validate(
        params    => [sig => $signature],
        signature => ["CanValidate sig"],
        );

    ok(!$failed && $answer->{sig} == $signature, "$name: $failed_msg");
}

sub test_register_can_invalid
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_can("CanFoo", "foo");

    $answer = $signature->validate(
        params    => [sig => $signature],
        signature => ["CanFoo sig"],
        );

    ok($failed && !$answer->{sig} && $failed_msg =~ /expected CanFoo/, "$name: $failed_msg");
}

sub test_register_regex
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_regex("IP4", '\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}\b');

    $answer = $signature->validate(
        params    => [ip => "127.0.0.1"],
        signature => ["IP4 ip"],
        );

    ok(!$failed && $answer->{ip} eq "127.0.0.1", "$name: $failed_msg");
}

sub test_register_regex_invalid
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_regex("IP4", '\b((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\.|$)){4}\b');

    $answer = $signature->validate(
        params    => [ip => "ABC"],
        signature => ["IP4 ip"],
        );

    ok($failed && !$answer->{ip} && $failed_msg =~ /invalid type of value/, "$name: $failed_msg");
}

sub test_register_enum_multi_values
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_enum("OneTwoThree", "1", "2", "3");

    $answer = $signature->validate(
        params    => [sig => 2],
        signature => ["OneTwoThree sig"],
        );

    ok(!$failed && $answer->{sig} == 2, "$name: $failed_msg");
}

sub test_register_enum_multi_invalid
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_enum("OneTwoThree", "1", "2", "3");

    $answer = $signature->validate(
        params    => [sig => 4],
        signature => ["OneTwoThree sig"],
        );

    ok($failed && !$answer->{sig} && $failed_msg =~ /invalid type of value/, "$name: $failed_msg");
}

sub test_register_enum_invalid_value
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_enum("Eleven", "11");

    $answer = $signature->validate(
        params    => [sig => "111"],
        signature => ["Eleven sig"],
        );

    ok($failed && !$answer->{sig} && $failed_msg =~ /invalid type of value/, "$name: $failed_msg");
}

sub test_register_enum_invalid_value2
{
    my $signature = shift;
    my $name = shift;
    my $answer;

    $signature->register_enum("Eleven", "11");

    $answer = $signature->validate(
        params    => [sig => "1"],
        signature => ["Eleven sig"],
        );

    ok($failed && !$answer->{sig} && $failed_msg =~ /invalid type of value/, "$name: $failed_msg");
}
$test_count += scalar grep /^test_/, keys(%main::);
plan tests => $test_count;
