#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature qw(:all);
use Types::Standard qw(Int Str);
use Data::Dumper;

# ---------------------------------------------------------------------------
# Goal: Demonstrate mixed positional/named parameter style with aliases.
#
#   - Positional params appear before the 'named:' marker.
#   - Named params appear after 'named:'.
#   - Aliases let callers use alternate parameter names.
# ---------------------------------------------------------------------------

our $Int = Int;
our $Str = Str;

my $sig = Params::Signature->new( param_style => mixed_style, fuzzy => 0 );

# ---------------------------------------------------------------------------
# Mixed signature: 'id' and 'name' are positional, 'title' and
# 'department' are named.  'title' has the alias 'role'.
# ---------------------------------------------------------------------------
sub create_user {
    my $params = $sig->validate(
        \@_,
        [ 'Int  id',
          'Str  name',
          'named:',
          'Str  title|role     = "Engineer"',
          'Str  department?',
          '...',
        ],
    );
    return $params;
}

# -- positional + named hashref ----------------------------------------------
my $u = create_user( 1001, 'Alice', { title => 'Manager', department => 'IT' } );
print "=== Positional + named hashref ===\n";
printf "  id=%d  name=%s  title=%s  dept=%s\n",
    $u->{id}, $u->{name}, $u->{title}, $u->{department};

# -- positional + named raw key/value pairs ----------------------------------
$u = create_user( 1002, 'Bob', title => 'Developer', department => 'Eng' );
print "\n=== Positional + raw key/value pairs ===\n";
printf "  id=%d  name=%s  title=%s  dept=%s\n",
    $u->{id}, $u->{name}, $u->{title}, $u->{department};

# -- Using alias 'role' instead of 'title' -----------------------------------
$u = create_user( 1003, 'Carol', role => 'Director' );
print "\n=== Using alias 'role' for 'title' ===\n";
printf "  id=%d  name=%s  title=%s  dept=%s\n",
    $u->{id}, $u->{name}, $u->{title}, $u->{department} // 'undef';

# -- Missing named param falls back to default -------------------------------
$u = create_user( 1004, 'Dave' );
print "\n=== Only positional (title gets default) ===\n";
printf "  id=%d  name=%s  title=%s  dept=%s\n",
    $u->{id}, $u->{name}, $u->{title}, $u->{department} // 'undef';

# -- note: passing all params in a single hashref does NOT work ----------
# -- with mixed_style — the hashref is treated as the next positional   --
# -- argument. Use the positional+hashref or positional+raw-kv patterns  --
# -- shown above.                                                        --

# -- Extra positional parameters allowed via '...' ---------------------------
print "\n=== Extra positional args (via '...') ===\n";
$u = create_user( 1005, 'Eve', 42, 'extra_info' );
print Dumper($u);

__END__

=head1 EXPECTED OUTPUT (approximately)

=== Positional + named hashref ===
  id=1001  name=Alice  title=Manager  dept=IT

=== Positional + raw key/value pairs ===
  id=1002  name=Bob  title=Developer  dept=Eng

=== Using alias 'role' for 'title' ===
  id=1003  name=Carol  title=Director  dept=undef

=== All-in-one hashref ===
  id=1004  name=Dave  title=Architect  dept=undef

=== Only positional (title gets default) ===
  id=1005  name=Eve  title=Engineer  dept=undef

=== Extra positional args (via '...') ===
$VAR1 = {
          'title' => 'Engineer',
          '42' => 'extra_info',
          'id' => 1005,
          'name' => 'Eve'
        };

# Note: When signature ends with named params, extra params after
# the positional cutoff are also treated as named key/value pairs.
# Here '42' becomes a named parameter key with value 'extra_info'.
