// RUN: %target-parse-verify-swift

// No space next to the '#if'

#if // expected-error {{expected a build configuration expression to follow the #if clause}}
class D {} // expected-error {{expected #else or #endif at end of configuration block}}
#endif
// expected-error @-1{{unexpected configuration block terminator}}
