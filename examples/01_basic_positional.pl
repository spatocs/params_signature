#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature qw(validate);
use Types::Standard qw(Int Str HashRef);
use Data::Dumper;

# Store type constraints in the caller's namespace so
# Params::Signature can resolve them by name.
our $Int     = Int;
our $Str     = Str;
our $HashRef = HashRef;

# ---------------------------------------------------------------------------
# Goal: Demonstrate simple positional parameter validation.
#   - string (required)
#   - integer (required)
#   - hashref (optional)
# ---------------------------------------------------------------------------

sub configure_service {
    my $params = validate(
        \@_,
        [ 'Str  service_name',
          'Int  port',
          'HashRef  options?',
          '...'
        ],
    );

    # In scalar context validate() returns a hash reference.
    return $params;
}

# -- Successful invocation (options supplied) --------------------------------
my $cfg = configure_service( 'web-proxy', 8080, { timeout => 30 } );
print "=== Successful call (all three args) ===\n";
print Dumper($cfg);

# -- Successful invocation (optional hashref omitted) ------------------------
$cfg = configure_service( 'dns-resolver', 53 );
print "\n=== Successful call (optional hashref omitted) ===\n";
print Dumper($cfg);

# -- Failed invocation caught via eval ---------------------------------------
eval {
    configure_service( 'broken', 'not-a-number' );
};
if ($@) {
    print "\n=== Expected failure ===\n";
    print "Validation error:\n    $@";
}

__END__

=head1 EXPECTED OUTPUT (approximately)

=== Successful call (all three args) ===
$VAR1 = {
          'options' => {
                         'timeout' => 30
                       },
          'service_name' => 'web-proxy',
          'port' => 8080
        };

=== Successful call (optional hashref omitted) ===
$VAR1 = {
          'service_name' => 'dns-resolver',
          'port' => 53
        };

=== Expected failure ===
Validation error:
    Params::Signature: port failed validation. Expected Int, got not-a-number ...