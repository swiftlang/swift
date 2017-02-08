// RUN: %target-typecheck-verify-swift

// With a space next to the '#if'
#if 
// expected-error@-1 {{expected a condition to follow #if}}
class C {} //expected-error {{expected #else or #endif at end of conditional compilation block}}
#endif  // expected-error {{unexpected conditional compilation block terminator}}
