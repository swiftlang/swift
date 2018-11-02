// RUN: %target-swift-frontend -typecheck -verify %s

func frob(x: inout Int) {}

func foo() {
  let x: Int // expected-note* {{}}

  x = 0

  _ = { frob(x: x) }() // expected-error{{'x' is a 'let'}}
  _ = { x = 0 }() // expected-error{{'x' is a 'let'}}
  _ = { frob(x: x); x = 0 }() // expected-error 2 {{'x' is a 'let'}}
}
