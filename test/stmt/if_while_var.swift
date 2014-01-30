// RUN: %swift -parse -verify %s

func foo() -> Int? { return .None }
func nonOptional() -> Int { return 0 }
func use(x: Int) {}
func modify(x: @inout Int) {}

if let x = foo() {
  use(x)
  modify(&x) // expected-error{{not a subtype of '@lvalue}}
}

use(x) // expected-error{{unresolved identifier 'x'}}

if var x = foo() {
  use(x)
  modify(&x)
}

use(x) // expected-error{{unresolved identifier 'x'}}

if let x = nonOptional() { } // expected-error{{bound value in a conditional binding must be of Optional type}}

class B {}
class D : B {}

let d: D? = .None
if let x: B = d { }

if let x {} // expected-error{{requires an initializer}}

// TODO poor recovery in these cases
if let {} // expected-error{{expected pattern}} expected-error{{unused closure}}
if let x = {} // expected-error{{'{' after 'if'}}

if let x = foo() {
} else {
  // TODO: more contextual error? "x is only available on the true branch"?
  use(x) // expected-error{{unresolved identifier 'x'}}
}

if let x = foo() {
  use(x)
} else if let y = foo() {
  use(x) // expected-error{{unresolved identifier 'x'}}
  use(y)
} else {
  use(x) // expected-error{{unresolved identifier 'x'}}
  use(y) // expected-error{{unresolved identifier 'y'}}
}

var opt: Int? = .None

if let x = opt {}
if var x = opt {}
