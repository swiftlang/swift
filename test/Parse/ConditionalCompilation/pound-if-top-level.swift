// RUN: %target-typecheck-verify-swift -D BLAH

// Check that if config statement has range properly nested in its parent
// EOF.

#if BLAH
// expected-error@+1{{expected #else or #endif at end of conditional compilation block}}
