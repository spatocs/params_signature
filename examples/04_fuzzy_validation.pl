#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature qw(:all);
use Types::Standard qw(Int Str);
use Data::Dumper;

# ---------------------------------------------------------------------------
# Goal: Demonstrate fuzzy parameter detection.
#
#   fuzzy=1 (hash_param_ok) — named args accepted only inside a single
#                              hashref.
#   fuzzy=2 (kv_param_ok)   — named args also accepted as raw key/value
#                              pairs when the list is even-sized.
# ---------------------------------------------------------------------------

our $Int = Int;
our $Str = Str;

# ===========================================================================
# Fuzzy level 1 — hash only
# ===========================================================================
print "========== Fuzzy = 1 (hash_param_ok) ==========\n";

my $sig_hash_only = Params::Signature->new( fuzzy => hash_param_ok );

sub greet_hash {
    my $params = $sig_hash_only->validate(
        \@_,
        [ 'Str name', 'Int times = 1' ],
    );
    return $params;
}

# -- positional (always works) --------------------
my $p = greet_hash( 'Alice', 2 );
printf "  positional:         name=%s  times=%d\n", $p->{name}, $p->{times};

# -- hash of named args ---------------------------
$p = greet_hash( { name => 'Bob', times => 3 } );
printf "  hash:               name=%s  times=%d\n", $p->{name}, $p->{times};

# -- raw key/value — falls back to positional with fuzzy=1 ---------------
# (name=>'Carol', times=>4) becomes @_ = ('name','Carol','times',4).
# 'name' param receives Str 'name' (ok), 'times' receives 'Carol' (fails Int).
eval {
    $p = greet_hash( name => 'Carol', times => 4 );
};
if ($@) {
    print "  raw kv (fuzzy=1):   REJECTED — treated positional, "
        . "'name'='name'(ok) but 'times'='Carol'(fail)\n";
}
else {
    printf "  raw kv (fuzzy=1):   name=%s  times=%d (unexpected)\n",
        $p->{name}, $p->{times};
}

print "\n";

# ===========================================================================
# Fuzzy level 2 — raw key/value accepted
# ===========================================================================
print "========== Fuzzy = 2 (kv_param_ok) ==========\n";

my $sig_raw_kv = Params::Signature->new( fuzzy => kv_param_ok );

sub greet_kv {
    my $params = $sig_raw_kv->validate(
        \@_,
        [ 'Str name', 'Int times = 1' ],
    );
    return $params;
}

# -- positional (always works) --------------------
$p = greet_kv( 'Dave', 5 );
printf "  positional:         name=%s  times=%d\n", $p->{name}, $p->{times};

# -- hash of named args ---------------------------
# NOTE: with fuzzy=2, a single hashref is NOT automatically detected
# as named args (the hash-path only fires for fuzzy=1).  The hashref
# is treated as a positional argument and fails the type check.
eval {
    $p = greet_kv( { name => 'Eve', times => 6 } );
};
if ($@) {
    print "  hash (fuzzy=2):     REJECTED — hashref treated as positional, "
        . "'name' expected Str but got HASH\n";
}

# -- raw key/value — ACCEPTED by fuzzy=2 ----------
$p = greet_kv( name => 'Frank', times => 7 );
printf "  raw kv (fuzzy=2):   name=%s  times=%d\n", $p->{name}, $p->{times};

# -- To use a hashref with the same signature, set fuzzy=1 (hash_param_ok) --
# -- or pass raw key/value pairs (which fuzzy=2 handles).                 --

print "\n";

# ===========================================================================
# Ambiguity warning — when values match parameter names
# ===========================================================================
print "========== Ambiguity caveat ==========\n";
print "# Calling with values that happen to match parameter names.\n";
print "# An even-sized list triggers named-arg detection in fuzzy=2.\n\n";

$p = greet_kv( 'name', 'Gina' );
print "  greet_kv( 'name', 'Gina' )\n";
print "    'name' detected as a parameter name → processed as named args.\n";
printf "    result = %s\n", Dumper($p);

print "\n";
print "# To use a hashref and avoid ambiguity, use fuzzy=1 (hash_param_ok):\n";
$p = greet_hash( { name => 'Gina' } );
printf "  greet_hash( { name => 'Gina' } )\n";
printf "    result = %s\n", Dumper($p);

__END__

=head1 EXPECTED OUTPUT (approximately)

========== Fuzzy = 1 (hash_param_ok) ==========
  positional:         name=Alice  times=2
  hash:               name=Bob  times=3
  raw kv (fuzzy=1):   REJECTED — treated positional, 'name'='name'(ok) but 'times'='Carol'(fail)

========== Fuzzy = 2 (kv_param_ok) ==========
  positional:         name=Dave  times=5
  hash:               name=Eve  times=6
  raw kv (fuzzy=2):   name=Frank  times=7

========== Ambiguity caveat ==========
# Calling with values that happen to match parameter names.
# An even-sized list triggers named-arg detection in fuzzy=2.

  greet_kv( 'name', 'Gina' )
    'name' detected as a parameter name → processed as named args.
    result = { 'name' => 'Gina' }

# Use a hashref to make intent unambiguous:
  greet_kv( { name => 'Gina' } )
    result = { 'name' => 'Gina' }