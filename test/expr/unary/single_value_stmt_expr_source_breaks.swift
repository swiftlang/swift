// RUN: %target-typecheck-verify-swift -enable-experimental-feature StatementExpressions

struct Err: Error {}

func foo() throws -> Int {
  // We actually do some constant evaluation before unreachability checking,
  // so this used to be legal.
  switch true {
  case true:
    throw Err()
  case false:
    () // expected-error {{cannot convert value of type '()' to specified type 'Int}}
  }
}

func bar() {
  // This used to be an unreachable 'if' after a return.
  return
    if .random() { 0 } else { 1 }
    // expected-error@-1 {{cannot convert value of type 'Int' to specified type '()'}}
}
