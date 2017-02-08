// RUN: %target-typecheck-verify-swift

struct NonOptionalStruct {}
enum NonOptionalEnum { case foo }

func foo() -> Int? { return .none }
func nonOptionalStruct() -> NonOptionalStruct { fatalError() }
func nonOptionalEnum() -> NonOptionalEnum { fatalError() }
func use(_ x: Int) {}
func modify(_ x: inout Int) {}

if let x = foo() {
  use(x)
  modify(&x) // expected-error{{cannot pass immutable value as inout argument: 'x' is a 'let' constant}}
}

use(x) // expected-error{{unresolved identifier 'x'}}

if var x = foo() {
  use(x)
  modify(&x)
}

use(x) // expected-error{{unresolved identifier 'x'}}

if let x = nonOptionalStruct() { } // expected-error{{initializer for conditional binding must have Optional type, not 'NonOptionalStruct'}}
if let x = nonOptionalEnum() { } // expected-error{{initializer for conditional binding must have Optional type, not 'NonOptionalEnum'}}

guard let _ = nonOptionalStruct() else { fatalError() } // expected-error{{initializer for conditional binding must have Optional type, not 'NonOptionalStruct'}}
guard let _ = nonOptionalEnum() else { fatalError() } // expected-error{{initializer for conditional binding must have Optional type, not 'NonOptionalEnum'}}

if case let x? = nonOptionalStruct() { } // expected-error{{'?' pattern cannot match values of type 'NonOptionalStruct'}}
if case let x? = nonOptionalEnum() { } // expected-error{{'?' pattern cannot match values of type 'NonOptionalEnum'}}

class B {} // expected-note * {{did you mean 'B'?}}
class D : B {}// expected-note * {{did you mean 'D'?}}

// TODO poor recovery in these cases
if let {} // expected-error {{expected '{' after 'if' condition}} expected-error {{pattern matching in a condition requires the 'case' keyword}}
if let x = {} // expected-error{{'{' after 'if'}} expected-error {{variable binding in a condition requires an initializer}} expected-error{{initializer for conditional binding must have Optional type, not '() -> ()'}}

if let x = foo() {
} else {
  // TODO: more contextual error? "x is only available on the true branch"?
  use(x) // expected-error{{unresolved identifier 'x'}}
}

if let x = foo() {
  use(x)
} else if let y = foo() { // expected-note {{did you mean 'y'?}}
  use(x) // expected-error{{unresolved identifier 'x'}}
  use(y)
} else {
  use(x) // expected-error{{unresolved identifier 'x'}}
  use(y) // expected-error{{unresolved identifier 'y'}}
}

var opt: Int? = .none

if let x = opt {} // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}}
if var x = opt {} // expected-warning {{value 'x' was defined but never used; consider replacing with boolean test}}

// <rdar://problem/20800015> Fix error message for invalid if-let
let someInteger = 1
if let y = someInteger {}  // expected-error {{initializer for conditional binding must have Optional type, not 'Int'}}
if case let y? = someInteger {}  // expected-error {{'?' pattern cannot match values of type 'Int'}}

// Test multiple clauses on "if let".
if let x = opt, let y = opt, x != y,
   let a = opt, var b = opt { // expected-warning {{immutable value 'a' was never used; consider replacing with '_' or removing it}} expected-warning {{variable 'b' was never used; consider replacing with '_' or removing it}}
}

// Leading boolean conditional.
if 1 != 2, let x = opt,
   y = opt,  // expected-error {{expected 'let' in conditional}} {{4-4=let }}
   x != y,
   let a = opt, var b = opt { // expected-warning {{immutable value 'a' was never used; consider replacing with '_' or removing it}} expected-warning {{variable 'b' was never used; consider replacing with '_' or removing it}}
}

// <rdar://problem/20457938> typed pattern is not allowed on if/let condition
if 1 != 2, let x : Int? = opt {} // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
// expected-warning @-1 {{explicitly specified type 'Int?' adds an additional level of optional to the initializer, making the optional check always succeed}} {{20-26=Int}}

if 1 != 2, case let x? : Int? = 42 {} // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}}
// expected-warning @-1 {{non-optional expression of type 'Int' used in a check for optionals}}



// Test error recovery.
// <rdar://problem/19939746> Improve error recovery for malformed if statements
if 1 != 2, { // expected-error {{'() -> ()' is not convertible to 'Bool'}}
} // expected-error {{expected '{' after 'if' condition}}
if 1 != 2, 4 == 57 {}
if 1 != 2, 4 == 57, let x = opt {} // expected-warning {{immutable value 'x' was never used; consider replacing with '_' or removing it}}

// Test that these don't cause the parser to crash.
if true { if a == 0; {} }   // expected-error {{expected '{' after 'if' condition}} expected-error 2{{}} expected-note 1{{}}
if a == 0, where b == 0 {}  // expected-error 3{{}} expected-note {{}} {{25-25=do }}




func testIfCase(_ a : Int?) {
  if case nil = a, a != nil {}
  if case let (b?) = a, b != 42 {}
  
  if let case (b?) = a, b != 42 {}  // expected-error {{pattern matching binding is spelled with 'case let', not 'let case'}} {{6-10=}} {{14-14= let}}
  
  if a != nil, let c = a, case nil = a { _ = c}
  
  if let p? = a {_ = p}  // expected-error {{pattern matching in a condition implicitly unwraps optionals}} {{11-12=}}
  
  
  if let .some(x) = a {_ = x}  // expected-error {{pattern matching in a condition requires the 'case' keyword}} {{6-6=case }}

  if case _ = a {}  // expected-warning {{'if' condition is always true}}
  while case _ = a {}  // expected-warning {{'while' condition is always true}}

}

// <rdar://problem/20883147> Type annotation for 'let' condition still expected to be optional
func testTypeAnnotations() {
  if let x: Int = Optional(1) {_ = x}
  if let x: Int = .some(1) {_ = x}

  if case _ : Int8 = 19 {}  // expected-warning {{'if' condition is always true}}
}

func testShadowing(_ a: Int?, b: Int?, c: Int?, d: Int?) {
  guard let a = a, let b = a > 0 ? b : nil else { return }
  _ = b

  if let c = c, let d = c > 0 ? d : nil {
    _ = d
  }
}

func useInt(_ x: Int) {}

func testWhileScoping(_ a: Int?) {// expected-note {{did you mean 'a'?}}
  while let x = a { }
  useInt(x) // expected-error{{use of unresolved identifier 'x'}}
}

