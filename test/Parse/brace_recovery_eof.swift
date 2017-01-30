// RUN: %target-typecheck-verify-swift

// Make sure source ranges satisfy the verifier.
for foo in [1, 2] { // expected-note {{to match this opening '{'}}
  _ = foo
// expected-error@+1{{expected '}' at end of brace statement}}
