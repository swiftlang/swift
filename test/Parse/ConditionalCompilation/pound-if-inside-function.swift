// RUN: %target-typecheck-verify-swift

// Check that if config statement has range properly nested in its parent
// EOF.

func foo() { // expected-note {{to match this opening '{'}}
#if BLAH // expected-astgen-error {{expected '#endif' in conditional compilation block}} expected-astgen-error {{expected '}' to end function}}
// expected-cparser-error@+2{{expected '}' at end of brace statement}}
// expected-cparser-error@+1{{expected #else or #endif at end of conditional compilation block}}
