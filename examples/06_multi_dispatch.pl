#!/usr/bin/env perl
use strict;
use warnings;
use FindBin;
use lib "$FindBin::Bin/../lib";

use Params::Signature::Multi;
use Types::Standard qw(Int Str);

our $Int = Int;
our $Str = Str;

# ---------------------------------------------------------------------------
# Goal: Demonstrate signature-based subroutine dispatch.
#        A single public subroutine delegates to one of several private
#        implementations based on which signature matches the arguments.
# ---------------------------------------------------------------------------

my $multi = Params::Signature::Multi->new();

# -- Implementation helpers --------------------------------------------------
sub _handle_with_message {
    my $params = shift;
    printf "  _handle_with_message: id=%d  message=%s  extra=%s\n",
        $params->{id}, $params->{message},
        join(', ', map { $params->{$_} } grep { !/^(id|message)$/ } keys %$params);
    return "with-message-ok";
}

sub _handle_simple {
    my $params = shift;
    printf "  _handle_simple:       id=%d\n", $params->{id};
    return "simple-ok";
}

sub _handle_fallback {
    my $params = shift;
    print  "  _handle_fallback:     unmatched arguments, all extra\n";
    return "fallback";
}

# -- Public dispatcher -------------------------------------------------------
sub process {
    return $multi->dispatch(
        \@_,
        [
            { signature => ['Int id', 'Str message', '...'],
              call      => \&_handle_with_message },
            { signature => ['Int id'],
              call      => \&_handle_simple },
            { signature => ['...'],
              call      => \&_handle_fallback },
        ],
    );
}

# -- Test cases --------------------------------------------------------------

print "=== process(42, 'hello', 'extra1', 'extra2') ===\n";
process(42, 'hello', 'extra1', 'extra2');

print "\n=== process(99) ===\n";
process(99);

print "\n=== process('unexpected') ===\n";
process('unexpected');

print "\n=== process() ===\n";
process();

__END__

=head1 EXPECTED OUTPUT (approximately)

=== process(42, 'hello', 'extra1', 'extra2') ===
  _handle_with_message: id=42  message=hello  extra=p_2, p_3

=== process(99) ===
  _handle_simple:       id=99

=== process('unexpected') ===
  _handle_fallback:     unmatched arguments, all extra

=== process() ===
  _handle_fallback:     unmatched arguments, all extra