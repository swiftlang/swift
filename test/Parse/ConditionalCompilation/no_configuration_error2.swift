// RUN: %target-typecheck-verify-swift

// No space next to the '#if'

#if
// expected-error@-1 {{expected a condition to follow #if}}
class D {} // expected-error {{expected #else or #endif at end of conditional compilation block}}
#endif
// expected-error@-1 {{unexpected conditional compilation block terminator}}
