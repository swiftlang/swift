// RUN: %swift -parse %s -verify

// No space next to the '#if'

// expected-error@+1{{expected expression, var, or let in 'if' condition}}
#if// expected-error 2{{invalid character in source file}}
class D {}
#endif
