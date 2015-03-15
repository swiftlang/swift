// RUN: %target-parse-verify-swift

func foo() -> Int? { return .None }
func nonOptional() -> Int { return 0 }
func use(x: Int) {}
func modify(inout x: Int) {}

if let x? = foo() {
  use(x)
  modify(&x) // expected-error{{cannot assign to immutable value of type 'Int'}}
}

use(x) // expected-error{{unresolved identifier 'x'}}

if var x? = foo() {
  use(x)
  modify(&x)
}

use(x) // expected-error{{unresolved identifier 'x'}}

if let x? = nonOptional() { } // expected-error{{optional present pattern cannot match values of type 'Int'}}

class B {}
class D : B {}

// TODO poor recovery in these cases
if let {} // expected-error {{expected '{' after 'if' condition}}
if let x? = {} // expected-error{{'{' after 'if'}} expected-error {{variable binding in a condition requires an initializer}}

if let x? = foo() {
} else {
  // TODO: more contextual error? "x is only available on the true branch"?
  use(x) // expected-error{{unresolved identifier 'x'}}
}

if let x? = foo() {
  use(x)
} else if let y? = foo() {
  use(x) // expected-error{{unresolved identifier 'x'}}
  use(y)
} else {
  use(x) // expected-error{{unresolved identifier 'x'}}
  use(y) // expected-error{{unresolved identifier 'y'}}
}

var opt: Int? = .None

if let x? = opt {}
if var x? = opt {}

// Test multiple clauses on "if let".
if let x? = opt, y? = opt where x != y,
   let a? = opt, var b? = opt {
}

// Leading boolean conditional.
if 1 != 2, let x? = opt, y? = opt where x != y,
   let a? = opt, var b? = opt {
}

// Test error recovery.
// <rdar://problem/19939746> Improve error recovery for malformed if statements
if 1 != 2, {  // expected-error {{expected 'let' or 'var' in conditional}}
}
if 1 != 2, 4 == 57 {}   // expected-error {{expected 'let' or 'var' in conditional; use '&&' to join boolean conditions}}{{10-11= &&}}
if 1 != 2, 4 == 57, let x? = opt {} // expected-error {{expected 'let' or 'var' in conditional; use '&&' to join boolean conditions}}

// Test that these don't cause the parser to crash.
if true { if a == 0; {} }   // expected-error {{expected '{' after 'if' condition}} expected-error 2{{}}
if a == 0, where b == 0 {}  // expected-error {{expected 'let' or 'var' in conditional; use '&&' to join boolean conditions}} expected-error 4{{}} expected-note {{}}


if let a = foo() {  // expected-warning {{condition requires a refutable pattern match; did you mean to match an optional?}}{{9-9=?}}
}


if let a : AnyObject = foo() { // expected-warning {{condition requires a refutable pattern match; did you mean to match an optional?}}{{9-9=?}}
                               // expected-error@-1 {{type annotation is not permitted in refutable match}}{{10-21=}}
  
}





