// RUN: %target-typecheck-verify-swift

#if arch(x86_64)
// expected-error@+2{{expected '{' in protocol type}}
// expected-error@+1{{extraneous '}' at top level}}
public protocol CS}
// expected-error@+1{{expected #else or #endif at end of conditional compilation block}}
