// RUN: %target-typecheck-verify-swift

// No space next to the '#if'

#if
// expected-error@-1 {{incomplete condition in conditional compilation directive}}
class D {}
#endif
// expected-error@-1 {{unexpected conditional compilation block terminator}}
