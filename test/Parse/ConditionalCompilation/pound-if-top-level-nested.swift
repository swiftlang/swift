// RUN: %target-typecheck-verify-swift

#if _runtime(_ObjC)
#if arch(x86_64)

// expected-error@+2{{expected #else or #endif at end of conditional compilation block}}
// expected-error@+1{{expected #else or #endif at end of conditional compilation block}}
