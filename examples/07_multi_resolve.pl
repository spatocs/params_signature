#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature::Multi;
use Types::Standard qw(Int Str HashRef);

our $Int     = Int;
our $Str     = Str;
our $HashRef = HashRef;

# ---------------------------------------------------------------------------
# Goal: Demonstrate resolve() — find the matching signature without calling
#        a pre-registered subroutine.  Useful for getter/setter patterns
#        where the caller wants to branch locally.
# ---------------------------------------------------------------------------

my $multi = Params::Signature::Multi->new();

# -- Simulated object store --------------------------------------------------
my %store;

sub accessor {
    my $self = shift;
    my ($idx, $id, $validated) = $multi->resolve(
        \@_,
        [
            { id => 'set',    signature => ['Str key', 'Str value'] },
            { id => 'get',    signature => ['Str key'] },
            { id => 'delete', signature => ['Str key', 'Str :$force?'] },
            { id => 'fallback', signature => ['...'] },
        ],
    );

    if ($id eq 'set') {
        $store{$validated->{key}} = $validated->{value};
        printf "  SET   key='%s'  value='%s'\n",
            $validated->{key}, $validated->{value};
        return $validated->{value};
    }
    elsif ($id eq 'get') {
        my $v = $store{$validated->{key}} // '[undef]';
        printf "  GET   key='%s'  value='%s'\n", $validated->{key}, $v;
        return $v;
    }
    elsif ($id eq 'delete') {
        my $forced = $validated->{force} ? ' forced' : '';
        printf "  DELETE key='%s'%s\n", $validated->{key}, $forced;
        return delete $store{$validated->{key}};
    }
    else {
        die "accessor: unrecognised signature\n";
        return;
    }
}

# -- Test cases --------------------------------------------------------------

print "=== SET ===\n";
accessor(undef, 'name', 'Alice');

print "\n=== GET ===\n";
accessor(undef, 'name');

print "\n=== SET another ===\n";
accessor(undef, 'db_host', 'pg.example.com');

print "\n=== DELETE with named force ===\n";
accessor(undef, 'db_host', force => 1);

print "\n=== GET after delete ===\n";
accessor(undef, 'db_host');

print "\n=== FALLBACK (no args) ===\n";
eval { accessor(undef) };
if ($@) {
    print "  Fallback caught (expected): $@";
}

__END__

=head1 EXPECTED OUTPUT (approximately)

=== SET ===
  SET   key='name'  value='Alice'

=== GET ===
  GET   key='name'  value='Alice'

=== SET another ===
  SET   key='db_host'  value='pg.example.com'

=== DELETE with named force ===
  DELETE key='db_host' forced

=== GET after delete ===
  GET   key='db_host'  value='[undef]'

=== FALLBACK (no args) ===
  Fallback caught (expected): ... at ...