#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature qw(validate);
use Types::Standard qw(Int Str ArrayRef HashRef);
use Data::Dumper;

# ---------------------------------------------------------------------------
# Goal: Showcase callbacks for fine-grained validation beyond type checks.
#        Simulates a JSON-like API webhook payload where individual elements
#        and their relationships must be validated.
# ---------------------------------------------------------------------------

our $Int     = Int;
our $Str     = Str;
our $ArrayRef = ArrayRef;
our $HashRef = HashRef;

# ---------------------------------------------------------------------------
# Helper: callback that checks sum of array elements does not exceed a limit.
sub sum_not_greater_than {
    my ($limit) = @_;
    return sub {
        my $value = shift;    # the actual parameter value
        my $sum = 0;
        $sum += $_->{id} for @$value;
        $sum <= $limit
          or die "sum of ids ($sum) exceeds limit ($limit)";
        return 1;
    };
}

# ---------------------------------------------------------------------------
# Helper: callback ensuring every array element is a hash with a "name" key.
sub all_have_name_key {
    return sub {
        my $value = shift;
        for my $item (@$value) {
            ref($item) eq 'HASH'
              and exists $item->{name}
              or die "array element missing 'name' key: " . Dumper($item);
        }
        return 1;
    };
}

# ---------------------------------------------------------------------------
# Webhook payload handler — validates an array of item hashes.
sub handle_webhook {
    my ($webhook_name, $items, $metadata) = @_;

    my $params = validate(
        \@_,
        [
            'Str      webhook_name',
            'ArrayRef items',
            'HashRef  metadata?',
        ],
        {
            callbacks => {
                webhook_name => {
                    'starts with hook_' => sub {
                        my ($v) = @_;
                        $v =~ /^hook_/
                          or die "'$v' must start with 'hook_'";
                        return 1;
                    },
                },
                items => {
                    'no more than 5 items'   => sub { @{$_[0]} <= 5 or die "too many items"; 1 },
                    'sum of ids <= 100'      => sum_not_greater_than(100),
                    'each has name key'      => all_have_name_key(),
                },
            },
        },
    );

    return $params;
}

# -- Successful payload ------------------------------------------------------
my @items = (
    { name => 'alpha', id => 10 },
    { name => 'beta',  id => 20 },
    { name => 'gamma', id => 30 },
);

eval {
    my $result = handle_webhook( 'hook_receiver', \@items, { source => 'api' } );
    print "=== Successful webhook ===\n";
    print Dumper($result);
};
if ($@) { print "Unexpected failure: $@\n"; }

# -- Failure: webhook_name does not match hook_ prefix ------------------------
eval {
    handle_webhook( 'bad_hook', \@items );
};
if ($@) {
    print "\n=== Failure: bad prefix ===\n";
    print "Error: $@";
}

# -- Failure: item missing mandatory "name" key ------------------------------
my @bad_items = (
    { id => 10 },     # no "name" key!
);
eval {
    handle_webhook( 'hook_broken', \@bad_items );
};
if ($@) {
    print "\n=== Failure: missing 'name' key ===\n";
    print "Error: $@";
}

# -- Failure: sum of ids exceeds limit ---------------------------------------
my @big_items = (
    { name => 'x', id => 50 },
    { name => 'y', id => 60 },    # sum = 110 > 100
);
eval {
    handle_webhook( 'hook_overflow', \@big_items );
};
if ($@) {
    print "\n=== Failure: sum of ids exceeds 100 ===\n";
    print "Error: $@";
}

__END__

=head1 EXPECTED OUTPUT (approximately)

=== Successful webhook ===
$VAR1 = {
          'webhook_name' => 'hook_receiver',
          'items' => [
                       { 'id' => 10, 'name' => 'alpha' },
                       { 'id' => 20, 'name' => 'beta'  },
                       { 'id' => 30, 'name' => 'gamma' }
                     ],
          'metadata' => { 'source' => 'api' }
        };

=== Failure: bad prefix ===
Error: Params::Signature: webhook_name failed validation via callback
       'starts with hook_': 'bad_hook' must start with 'hook_' ...

=== Failure: missing 'name' key ===
Error: Params::Signature: items failed validation via callback
       'each has name key': array element missing 'name' key ...

=== Failure: sum of ids exceeds 100 ===
Error: Params::Signature: items failed validation via callback
       'sum of ids <= 100': sum of elements (110) exceeds limit (100) ...