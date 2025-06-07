// RUN: %target-typecheck-verify-swift

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
    // expected-error@-1 {{unexpected non-void return value in void function}}
    // expected-note@-2 {{did you mean to add a return type?}}
}
