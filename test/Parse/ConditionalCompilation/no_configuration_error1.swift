// RUN: %target-typecheck-verify-swift

// With a space next to the '#if'
#if 
// expected-cparser-error@-1 {{incomplete condition in conditional compilation directive}} expected-astgen-error@-1 {{invalid conditional compilation expression}}
class C {}
#endif  // expected-cparser-error {{unexpected conditional compilation block terminator}}
