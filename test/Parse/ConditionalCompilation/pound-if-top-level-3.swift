// RUN: %target-typecheck-verify-swift

#if arch(x86_64)
// expected-cparser-error@+2{{expected '{' in protocol type}} expected-astgen-error@+2{{expected '#endif' in conditional compilation block}}
// expected-cparser-error@+1{{extraneous '}' at top level}} expected-astgen-error@+1{{expected member block in protocol}} expected-astgen-error@+1{{unexpected brace in conditional compilation clause}}
public protocol CS}
// expected-cparser-error@+1{{expected #else or #endif at end of conditional compilation block}}
