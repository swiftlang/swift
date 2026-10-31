// RUN: %target-typecheck-verify-swift

// No space next to the '#if'

#if
// expected-cparser-error@-1 {{incomplete condition in conditional compilation directive}} expected-astgen-error@-1 {{invalid conditional compilation expression}}
class D {}
#endif
// expected-cparser-error@-1 {{unexpected conditional compilation block terminator}}
