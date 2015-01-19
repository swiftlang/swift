// RUN: %target-parse-verify-swift

// With a space next to the '#if'
#if // expected-error {{expected a build configuration expression to follow the #if clause}}
class C {} //expected-error {{expected #else or #endif at end of configuration block}}
#endif  // expected-error {{unexpected configuration block terminator}}
