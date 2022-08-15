// RUN: %target-typecheck-verify-swift

func frob(x: inout Int) {}

func foo() {
  let x: Int // expected-note* {{}}

  x = 0

  _ = { frob(x: x) }() // expected-error{{'x' is a 'let'}}
  _ = { x = 0 }() // expected-error{{'x' is a 'let'}}
  _ = { frob(x: x); x = 0 }() // expected-error {{'x' is a 'let'}}
}

let a: Int 
{ 1 } // expected-error{{'let' declarations cannot be computed properties}}

let b: Int = 1
{ didSet { print("didSet") } } // expected-error{{'let' declarations cannot be observing properties}}
