// RUN: %target-typecheck-verify-swift -D BLAH

// Check that if config statement has range properly nested in its parent
// EOF.

func foo() { // expected-note {{to match this opening '{'}}
#if BLAH
  _ = 123
#elseif !BLAH
#else
#else // expected-error{{further conditions after #else are unreachable}}
// expected-error@+2{{expected '}' at end of brace statement}}
// expected-error@+1{{expected #else or #endif at end of conditional compilation block}}
