// RUN: %target-parse-verify-swift

#if arch(x86_64)
// expected-error@+2{{expected '{' in protocol type}}
// expected-error@+1{{expected #else or #endif at end of configuration block}}
public protocol CS}
