// RUN: %target-parse-verify-swift

func foo() -> Int? { return .None }
func nonOptional() -> Int { return 0 }
func use(x: Int) {}
func modify(inout x: Int) {}

if let x = foo() {
  use(x)
  modify(&x) // expected-error{{cannot assign to immutable value of type 'Int'}}
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

if let x {} // expected-error{{requires an initializer}}

// TODO poor recovery in these cases
if let {} // expected-error{{expected pattern}} expected-error{{unused closure}} expected-error{{begin with a closure}} expected-note{{discard the result}} expected-error{{type of expression is ambiguous without more context}}
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

// Test multiple clauses on "if let".
if let x = opt, y = opt where x != y,
   let a = opt, var b = opt {
}

// Leading boolean conditional.
if 1 != 2, let x = opt, y = opt where x != y,
   let a = opt, var b = opt {
}

// Test error recovery.
// <rdar://problem/19939746> Improve error recovery for malformed if statements
if 1 != 2, {  // expected-error {{expected 'let' or 'var' in conditional}}
}
if 1 != 2, 4 == 57 {}   // expected-error {{expected 'let' or 'var' in conditional; use '&&' to join boolean conditions}}
if 1 != 2, 4 == 57, let x = opt {} // expected-error {{expected 'let' or 'var' in conditional; use '&&' to join boolean conditions}}

// Test that these don't cause the parser to crash.
if true { if a == 0; {} }   // expected-error {{expected '{' after 'if' condition}} expected-error 2{{}}
if a == 0, where b == 0 {}  // expected-error {{expected 'let' or 'var' in conditional; use '&&' to join boolean conditions}} expected-error 4{{}} expected-note {{}}








