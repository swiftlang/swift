// RUN: %swift -D BLAH %s -verify -parse

// Check that if config statement has range properly nested in its parent
// EOF.

#if BLAH
// expected-error@+1{{expected #else or #endif at end of configuration block}}
