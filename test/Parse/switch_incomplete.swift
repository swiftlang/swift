// RUN: %target-typecheck-verify-swift

// <rdar://problem/15971438> Incomplete switch was parsing to an AST that
// triggered an assertion failure.
switch 1 { // expected-note{{to match this opening '{'}}
case 1:    // expected-error@+1{{expected '}' at end of 'switch' statement}}
