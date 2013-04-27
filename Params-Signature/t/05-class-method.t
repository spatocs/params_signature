#!perl 
use 5.006;
use strict;
use warnings FATAL => 'all';
use Test::More;

use Data::Dumper;
use Try::Tiny;
use Params::Signature;

my $test_count = 0;

my @test_criteria = (
    { name => "Pos: 3 required", args => [1,2,3], signature => ["Int one", "Int two", "Int three"], ok => 1, array_answer => [1,2,3] },
    { name => "Pos: 3 required, 1 missing", args => [1,2,], signature => ["Int one", "Int two", "Int three"], ok => 0 , array_answer => [1,2,]},
    { name => "Pos: 2 optional set", args => [1,2,], signature => ["Int one?", "Int two?", ], ok => 1, array_answer => [1,2,]},
    { name => "Pos: 0 of 2 optional set", args => [], signature => ["Int one?", "Int two?", ], ok => 1 },
    { name => "Pos: 1 of 2 optional set", args => [1], signature => ["Int one?", "Int two?", ], ok => 1 },
    { name => "Pos: 3 required, not set, with default", args => [], signature => ["Int one = 1", "Int two = 2", "Int three = 3"], ok => 1 },

    { name => "Pos: 3 required, not set, no default", args => [], signature => ["Int one", "Int two", "Int three"], ok => 0 },
    { name => "Pos: 3 required, not set, invalid default", args => [], signature => ["Int one = 'one'", "Int two = 'two'", "Int three = 'three'"], ok => 0 },
    { name => "Pos: 3 optional, 1 set, 2 depends fields with defaults", args => [1], signature => ["Int one? << [two, three]", "Int two? = 2", "Int three? = 3"], ok => 1, array_answer => [1,2,3] },
    { name => "Pos: 3 optional, 1 set, 1 depends field with default", args => [1], signature => ["Int one? << [three]", "Int two? = 2", "Int three? = 3"], ok => 1, array_answer => [1, undef, 3]},
    { name => "Pos: 3 optional, 0 set, 1 depends field with default", args => [], signature => ["Int one? << [three]", "Int two? = 2", "Int three? = 3"], ok => 1, array_answer => [], hash_answer => {} },
    { name => "Pos: 3 optional, 0 set, 2 depends field with defaults", args => [], signature => ["Int one? << [three, two]", "Int two? = 2", "Int three? = 3"], ok => 1, array_answer => [], hash_answer => {} },
    { name => "Pos: 2 required, 1 set,  1 with param default", args => [1], signature => ["Int one", "Int two <= one"], ok => 1, array_answer => [1,1], hash_answer => {} },
    { name => "Pos: 2 optional, 0 set, 1 with param default", args => [], signature => ["Int one?", "Int two? <= one"], ok => 1, array_answer => [], hash_answer => {} },
    { name => "Pos: 1 required, 0 set, 1 with sub default", args => [], signature => ["Int one = { 4*1 }" ], ok => 1, array_answer => [4], hash_answer => {one => 4} },
    { name => "Pos: 1 required, 0 set, 1 with sub default", args => [], signature => ["Int one = { 4*1 }" ], ok => 1, hash_answer => {one => 4} },
    { name => "Pos: 1 required, 1 optional, 1 set, 1 with sub default", args => [1], signature => ["Int one" , "Int two? = { 2*2 }"], ok => 1, hash_answer => {one => 1} },
    { name => "Pos: 1 required, 2 optional, 1 set, 1 with sub default", args => [1], signature => ["Int one" , "Int two? = { 2*2 }", "Int three?"], ok => 1, array_answer => [1] },
    { name => "Pos: 1 required, 1 extra with extra", args => [1,2], signature => ["Int one", "..."], ok => 1, array_answer => [1, 2] },
    { name => "Pos: 0 required, 2 extra with extra", args => [1,2], signature => ["..."], ok => 1, array_answer => [1, 2] },
    { name => "Pos: 3 optional, 1 set, 2 depends fields with defaults, use alias", args => [1], signature => ["Int one|uno? << [dos, tres]", "Int two|dos? = 2", "Int three|tres? = 3"], ok => 1, array_answer => [1,2,3] },

    { name => "Mixed: 2 required, 2 set, no default", args => [1, two => 2], signature => ["Int one" , "named:", "Int two"], ok => 1, array_answer => [1, two => 2] },
    { name => "Mixed: 2 required, 2 set, 1 pos, 1 named, no default", args => [1, two => 2], signature => ["Int one" , "named:", "Int two"], ok => 1, hash_answer => {one => 1, two => 2} },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named, no default", args => [1, 2, three => 3], signature => ["Int one" , "Int two", "named:", "Int three" ], ok => 1, hash_answer => {one => 1, two => 2, three => 3} },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named, no default", args => [1, 2, three => 3], signature => ["Int one" , "Int two", "named:", "Int three" ], ok => 1, array_answer => [1, 2, three => 3] },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named in hash, no default", args => [1, 2, {three => 3}], signature => ["Int one" , "Int two", "named:", "Int three" ], ok => 1, hash_answer => {one => 1, two => 2, three => 3} },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named in hash, no default", args => [1, 2, {three => 3}], signature => ["Int one" , "Int two", "named:", "Int three" ], ok => 1, array_answer => [1, 2, three => 3] },
    { name => "Mixed: 4 required, 4 set, 2 pos, 2 named in hash, no default", args => [1, 2, {three => 3, four => 4}], signature => ["Int one" , "Int two", "named:", "Int three", "Int four" ], ok => 1, hash_answer => {one => 1, two => 2, three => 3, four => 4} },
    { name => "Mixed: 4 required, 4 set, 2 pos, 2 named in hash, no default", args => [1, 2, {three => 3, four => 4}], signature => ["Int one" , "Int two", "named:", "Int three", "Int four" ], ok => 1, array_answer => [1, 2, three => 3, four => 4] },
    { name => "Mixed: 4 required, 4 set, 2 pos, 1 named in hash, 1 named alone, no default", args => [1, 2, {three => 3}, four => 4], signature => ["Int one" , "Int two", "named:", "Int three", "Int four" ], ok => 0, },
    { name => "Mixed: 2 required, 2 set, no default, use alias", args => [1, dos => 2], signature => ["Int one" , "named:", "Int two|dos"], ok => 1, array_answer => [1, two => 2] },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named, no default", args => [1, 2, three => 3], signature => ["Int one" , "Int two", 'Int :$three' ], ok => 1, hash_answer => {one => 1, two => 2, three => 3} },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named, no default, array answer", args => [1, 2, three => 3], signature => ["Int one" , "Int two", 'Int :$three' ], ok => 1, array_answer => [1, 2, three => 3] },
    { name => "Mixed: 3 required, 3 set, 2 pos, 1 named, no default, 2nd pos after named", args => [1, two => 2, 3], signature => ["Int one" , 'Int :$two', 'Int $three' ], ok => 0, msg_regex => "cannot appear after a named parameter" },
    { name => "Mixed: 2 required, 3 set, 1 pos, 1 named, 1 extra, array answer", args => [1, two => 2, three => 3], signature => ["Int one" , "named:", "Int two", '...' ], ok => 1, array_answer => [1, two => 2, three => 3] },

    { name => "Named: 2 optional set, args are hash, not array ref", args => {one => 1, two => 2,}, signature => ["named:", "Int one?", "Int two?", ], ok => 0 },

    { name => "Named: 4 required, 4 set, names missing", args => [1, 2, 3, 4], signature => ["named:", "Int one", "Int two", "Int three", "Int four"], ok => 0, msg_regex => "does not exist in the signature" },
    { name => "Named: 3 required", args => [one => 1, two => 2, three => 3], signature => ["named:", "Int one", "Int two", "Int three"], ok => 1 },
    { name => "Named: 3 required, as hash", args => [{one => 1, two => 2, three => 3}], signature => ["named:", "Int one", "Int two", "Int three"], ok => 1 },
    { name => "Named: 3 required, as hash, use alias", args => [{uno => 1, dos => 2, tres => 3}], signature => ["named:", "Int one|uno", "Int two|dos", "Int three|tres"], ok => 1, hash_answer => {one => 1, two => 2, three => 3} },

    { name => "Named: 3 required, 1 missing", args => [one => 1, two => 2,], signature => ["named:", "Int one", "Int two", "Int three"], ok => 0 },
    { name => "Named: 3 required, 1 missing, as hash", args => [{one => 1, two => 2,}], signature => ["named:", "Int one", "Int two", "Int three"], ok => 0 },

    { name => "Named: 2 optional set", args => [one => 1, two => 2,], signature => ["named:", "Int one?", "Int two?", ], ok => 1 },
    { name => "Named: 2 optional set", args => [{one => 1, two => 2}], signature => ["named:", "Int one?", "Int two?", ], ok => 1 },

    { name => "Named: 0 of 2 optional set", args => [], signature => ["named:", "Int one?", "Int two?", ], ok => 1 },
    { name => "Named: 1 of 2 optional set", args => [two => 2], signature => ["named:", "Int one?", "Int two?", ], ok => 1 },
    { name => "Named: 3 required, not set, with default", args => [], signature => ["named:", "Int one = 1", "Int two = 2", "Int three = 3"], ok => 1 },
    { name => "Named: 3 required, not set, with default", args => [], signature => ["named:", "Int one = 1", "Int two = 2", "Int three = 3"], ok => 1, array_answer => [1,2,3]},

    { name => "Named: 3 required, not set, no default", args => [], signature => ["named:", "Int one", "Int two", "Int three"], ok => 0 },
    { name => "Named: 3 required, not set, invalid default", args => [], signature => ["named:", "Int one = 'one'", "Int two = 'two'", "Int three = 'three'"], ok => 0, msg_regex => 'invalid type of value'},
    { name => "Named: 3 optional, 1 set, 2 depends fields with defaults", args => [one => 1], signature => ["named:", "Int one? << [two, three]", "Int two? = 2", "Int three? = 3"], ok => 1 },
    { name => "Named: 3 optional, 1 set, 1 depends field with default", args => [one => 1], signature => ["named:", "Int one? << [three]", "Int two? = 2", "Int three? = 3"], ok => 1, hash_answer => {one => 1, three => 3}},
    { name => "Named: 3 optional, 1 set, 1 depends field with default, array answer", args => [one => 1], signature => ["named:", "Int one? << [three]", "Int two? = 2", "Int three? = 3"], ok => 1, array_answer => [1, undef, 3]},
    { name => "Named: 3 optional, 0 set, 1 depends field with default", args => [], signature => ["named:", "Int one? << [three]", "Int two? = 2", "Int three? = 3"], ok => 1, hash_answer => {} },
    { name => "Named: 3 optional, 0 set, 2 depends field with defaults", args => [], signature => ["named:", "Int one? << [three, two]", "Int two? = 2", "Int three? = 3"], ok => 1, hash_answer => {} },
    { name => "Named: 2 required, 1 set,  1 with param default", args => [one => 1], signature => ["named:", "Int one", "Int two <= one"], ok => 1, hash_answer => {one => 1, two => 1} },
    { name => "Named: 2 required, 1 set,  1 with param default", args => [one => 1], signature => ["named:", "Int one", "Int two <= one"], ok => 1, array_answer => [1, 1] },

    { name => "Named: 2 optional, 0 set, 1 with param default", args => [], signature => ["named:", "Int one?", "Int two? <= one"], ok => 1,  hash_answer => {} },
    { name => "Named: 1 required, 0 set, 1 with sub default", args => [], signature => ["named:", "Int one = { 2*2 }" ], ok => 1, hash_answer => {one => 4} },
    { name => "Named: 1 required, 1 optional, 1 set, 1 with sub default", args => [one => 1], signature => ["named:", "Int one" , "Int two? = { 2*2 }"], ok => 1, hash_answer => {one => 1} },
    { name => "Named: 3 required, 1 extra param, as array", args => [one => 1, two => 2, three => 3, four => 4], signature => ["named:", "Int one", "Int two", "Int three"], ok => 0, msg_regex => "unexpected extra parameter" },

);
my @test_fuzzy_criteria = (
    { name => "fuzzy Named: 3 required, as hash", args => [{one => 1, two => 2, three => 3}], signature => ["Int one", "Int two", "Int three"], ok => 1 },
    { name => "fuzzy Named: 3 required, as array", args => [one => 1, two => 2, three => 3], signature => ["Int one", "Int two", "Int three"], ok => 1, hash_answer => {one => 1, two => 2, three => 3} },
    { name => "fuzzy Named: 2 required, 1 optional, required present in array", args => [one => 1, two => 2 ], signature => ["Int one", "Int two", "Int three?"], ok => 1, hash_answer => {one => 1, two => 2} },
    { name => "fuzzy Pos: 3 required, as array", args => [1, 2, 3], signature => ["Int one", "Int two", "Int three"], ok => 1, array_answer => [1, 2, 3] },
);

# the Signature module defaults to positional arguments, so
# any tests involving positional arguments should work
my @test_fuzzy_criteria_std_signature = (
    { name => "fuzzy Named: 3 required, as hash", args => [{one => 1, two => 2, three => 3}], signature => ["Int one", "Int two", "Int three"], ok => 0 },
    { name => "fuzzy Named: 3 required, as array", args => [one => 1, two => 2, three => 3], signature => ["Int one", "Int two", "Int three"], ok => 0, hash_answer => {one => 1, two => 2, three => 3} },
    { name => "fuzzy Pos: 3 required, as array", args => [1, 2, 3], signature => ["Int one", "Int two", "Int three"], ok => 1, array_answer => [1, 2, 3] },
);

plan tests => (scalar @test_criteria * 2) + (scalar @test_fuzzy_criteria) + (scalar @test_fuzzy_criteria_std_signature);

my $failed;
my $failed_msg;
sub catch_failed
{
    $failed_msg = shift;
    $failed = 1;
}

Main:
{
    my $criteria;
    my $answer;
    my @array_answer;
    my $idx;
    my $key;

    diag( "Perform argument validation using parameter signature, Perl $], $^X" );

    Params::Signature->class_default->{on_fail} = \&catch_failed;
    diag("Standard signature, standard criteria");
    process_test_criteria(Params::Signature->class_default, @test_criteria);
    diag("Fuzzy signature, standard criteria");
    Params::Signature->class_default->{fuzzy} = 1;
    process_test_criteria(Params::Signature->class_default, @test_criteria);
    diag("Fuzzy signature, fuzzy criteria");
    process_test_criteria(Params::Signature->class_default, @test_fuzzy_criteria);
    diag("Standard signature, fuzzy criteria");
    map { $_->{ok} = 0 } @test_fuzzy_criteria;
    Params::Signature->class_default->{fuzzy} = 0;
    process_test_criteria(Params::Signature->class_default, @test_fuzzy_criteria_std_signature);
}

sub process_test_criteria
{
    my $signature = shift;
    my @local_test_criteria = @_;
    my $criteria;
    my $answer;
    my @array_answer;
    my $idx;
    my $key;

    foreach $criteria (@local_test_criteria)
    {
        $failed = 0;
        $failed_msg = "";
        if ($criteria->{array_answer})
        {
            @array_answer = Params::Signature->validate($criteria->{args}, $criteria->{signature});
            #diag("array_answer:" . Dumper(\@array_answer));
            if (!$failed)
            {
                foreach $idx (0 .. (scalar @{$criteria->{array_answer}} - 1))
                {
                    # disable warnings because some values are expected to be undef
                    no warnings;
                    if ($criteria->{array_answer}[$idx] ne $array_answer[$idx])
                    {
                        $failed = 1;
                        $failed_msg = "Item $idx in array answer ($array_answer[$idx]) is not the expected value ($criteria->{array_answer}[$idx])";
                        last;
                    }
                }
            }
        }
        else
        {
            $answer = Params::Signature->validate($criteria->{args}, $criteria->{signature});
            #diag("answer:" . Dumper($answer));
            if (!$failed)
            {
                foreach $key (keys %{$criteria->{hash_answer}})
                {
                    # disable warnings because some values are expected to be undef
                    no warnings;
                    if ($criteria->{hash_answer}{$key} ne $answer->{$key})
                    {
                        $failed = 1;
                        $failed_msg = "Item $key in hash answer ($answer->{$key}) is not the expected value ($criteria->{hash_answer}{$key}";
                        last;
                    }
                }
            }
        }
        if (($criteria->{msg_regex}) &&
            ($failed_msg !~ /$criteria->{msg_regex}/))
        {
            ok(0, "$criteria->{name}: Failed for wrong reason: '$failed_msg', Expected to see: $criteria->{msg_regex}");
        }
        ok(!$failed == $criteria->{ok}, "$criteria->{name}: $failed_msg");
    }
}


__DATA__

    foreach $criteria (@test_criteria)
    {
        $failed = 0;
        $failed_msg = "";
        if ($criteria->{array_answer})
        {
            @array_answer = $signature->validate($criteria->{args}, $criteria->{signature});
            #diag("array_answer:" . Dumper(\@array_answer));
            if (!$failed)
            {
                foreach $idx (0 .. (scalar @{$criteria->{array_answer}} - 1))
                {
                    # disable warnings because some values are expected to be undef
                    no warnings;
                    if ($criteria->{array_answer}[$idx] ne $array_answer[$idx])
                    {
                        $failed = 1;
                        $failed_msg = "Item $idx in answer ($array_answer[$idx]) is not the expected value ($criteria->{array_answer}[$idx])";
                        last;
                    }
                }
            }
        }
        else
        {
            $answer = $signature->validate($criteria->{args}, $criteria->{signature});
            #diag("answer:" . Dumper($answer));
            if (!$failed)
            {
                foreach $key (keys %{$criteria->{hash_answer}})
                {
                    # disable warnings because some values are expected to be undef
                    no warnings;
                    if ($criteria->{hash_answer}{$key} ne $answer->{$key})
                    {
                        $failed = 1;
                        $failed_msg = "Item $key in answer ($answer->{$key}) is not the expected value ($criteria->{hash_answer}{$key}";
                        last;
                    }
                }
            }
        }
        if (($criteria->{msg_regex}) &&
            ($failed_msg !~ /$criteria->{msg_regex}/))
        {
            ok(0, "$criteria->{name}: Failed for wrong reason: '$failed_msg', Expected to see: $criteria->{msg_regex}");
        }
        ok(!$failed == $criteria->{ok}, "$criteria->{name}: $failed_msg");
    }

    foreach $criteria (@test_criteria)
    {
        $failed = 0;
        $failed_msg = "";
        if ($criteria->{array_answer})
        {
            @array_answer = $fuzzy_signature->validate($criteria->{args}, $criteria->{signature});
            #diag("array_answer:" . Dumper(\@array_answer));
            if (!$failed)
            {
                foreach $idx (0 .. (scalar @{$criteria->{array_answer}} - 1))
                {
                    # disable warnings because some values are expected to be undef
                    no warnings;
                    if ($criteria->{array_answer}[$idx] ne $array_answer[$idx])
                    {
                        $failed = 1;
                        $failed_msg = "Item $idx in answer ($array_answer[$idx]) is not the expected value ($criteria->{array_answer}[$idx])";
                        last;
                    }
                }
            }
        }
        else
        {
            $answer = $fuzzy_signature->validate($criteria->{args}, $criteria->{signature});
            #diag("answer:" . Dumper($answer));
            if (!$failed)
            {
                foreach $key (keys %{$criteria->{hash_answer}})
                {
                    # disable warnings because some values are expected to be undef
                    no warnings;
                    if ($criteria->{hash_answer}{$key} ne $answer->{$key})
                    {
                        $failed = 1;
                        $failed_msg = "Item $key in answer ($answer->{$key}) is not the expected value ($criteria->{hash_answer}{$key}";
                        last;
                    }
                }
            }
        }
        if (($criteria->{msg_regex}) &&
            ($failed_msg !~ /$criteria->{msg_regex}/))
        {
            ok(0, "$criteria->{name}: Failed for wrong reason: '$failed_msg', Expected to see: $criteria->{msg_regex}");
        }
        ok(!$failed == $criteria->{ok}, "$criteria->{name}: $failed_msg");
    }
