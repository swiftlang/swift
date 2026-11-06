// RUN: %target-typecheck-verify-swift

#if _runtime(_ObjC)
#if arch(x86_64) // expected-astgen-error {{expected '#endif' in conditional compilation block}} expected-astgen-error {{expected '#endif' in conditional compilation block}}

// expected-cparser-error@+2{{expected #else or #endif at end of conditional compilation block}}
// expected-cparser-error@+1{{expected #else or #endif at end of conditional compilation block}}
