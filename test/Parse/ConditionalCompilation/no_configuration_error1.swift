// RUN: %target-typecheck-verify-swift

// With a space next to the '#if'
#if 
// expected-error@-1 {{incomplete condition in conditional compilation directive}}
class C {}
#endif  // expected-error {{unexpected conditional compilation block terminator}}
