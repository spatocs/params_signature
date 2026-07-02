#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature qw(:all);
use Types::Standard qw(Int Str HashRef ArrayRef Bool Enum);
use Data::Dumper;

# ---------------------------------------------------------------------------
# Goal: Demonstrate named parameter (hash-style) validation with default
#        values and type constraints.  Simulates a database connection
#        configuration routine.
# ---------------------------------------------------------------------------

# Register custom types in the caller's namespace.
our $Int     = Int;
our $Str     = Str;
our $HashRef = HashRef;
our $Bool    = Bool;

# Singleton validator pre-configured for named-only parameters.
our $Validator = Params::Signature->new(
    param_style => named_style,
    coerce      => 1,
);

sub db_connect {
    my $params = $Validator->validate(
        \@_,
        [ "Str    host        = 'localhost'",
          'Int    port        = 5432',
          "Str    database    = 'myapp'",
          "Str    user        = 'nobody'",
          'Str    password?',
          'HashRef options?',
          'Bool   ssl         = 1',
          '...',
        ],
    );
    return $params;
}

# -- All defaults ------------------------------------------------------------
my $cfg = db_connect( {} );
print "=== All defaults ===\n";
print Dumper($cfg);

# -- Override a few fields ---------------------------------------------------
$cfg = db_connect( {
    host     => 'db.example.com',
    port     => 15432,
    database => 'production',
    user     => 'sandor',
} );
print "\n=== Overrides (host/port/db/user) ===\n";
print Dumper($cfg);

# -- With extra options hash -------------------------------------------------
$cfg = db_connect( {
    host     => 'pg.internal',
    options  => { RaiseError => 1, AutoCommit => 0 },
    password => 'secret',
} );
print "\n=== With options hash and password ===\n";
print Dumper($cfg);

# -- Successful call, but ssl is disabled ------------------------------------
$cfg = db_connect( {
    host => 'insecure.local',
    ssl  => 0,
} );
print "\n=== SSL disabled ===\n";
print Dumper($cfg);

# -- Failed: port must be an Int ---------------------------------------------
eval {
    db_connect( { host => 'bad', port => 'not-a-port' } );
};
if ($@) {
    print "\n=== Expected failure for bad port type ===\n";
    print "Validation error:\n    $@";
}

# -- Failed: unknown host type ------------------------------------------------
eval {
    db_connect( { host => { nested => 'hash' } } );
};
if ($@) {
    print "\n=== Expected failure for hash-as-host ===\n";
    print "Validation error:\n    $@";
}

__END__

=head1 EXPECTED OUTPUT (approximately)

=== All defaults ===
$VAR1 = {
          'ssl' => 1,
          'host' => 'localhost',
          'port' => 5432,
          'database' => 'myapp',
          'user' => 'nobody'
        };

=== Overrides (host/port/db/user) ===
$VAR1 = {
          'ssl' => 1,
          'host' => 'db.example.com',
          'port' => 15432,
          'database' => 'production',
          'user' => 'sandor'
        };

=== With options hash and password ===
$VAR1 = {
          'ssl' => 1,
          'host' => 'pg.internal',
          'port' => 5432,
          'database' => 'myapp',
          'user' => 'nobody',
          'password' => 'secret',
          'options' => {
                         'AutoCommit' => 0,
                         'RaiseError' => 1
                       }
        };

=== SSL disabled ===
$VAR1 = {
          'ssl' => 0,
          'host' => 'insecure.local',
          'port' => 5432,
          'database' => 'myapp',
          'user' => 'nobody'
        };

=== Expected failure for bad port type ===
Validation error:
    ... port failed validation. Expected Int, got not-a-port

=== Expected failure for hash-as-host ===
Validation error:
    ... host failed validation. Expected Str, got HASH(...)