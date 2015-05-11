// RUN: %target-parse-verify-swift

// ---------------------------------------------------------------------------
// Warnings about unused results
// ---------------------------------------------------------------------------
@warn_unused_result func f1() -> [Int] { }

func testFuncsNegative() {
  let x = f1()
  let _ = f1()
  _ = f1()
  for _ in f1() { }
  _ = x
}

func testFuncsPositive() {
  f1() // expected-warning{{result of call to 'f1()' is unused}}
}

class C1 {
  @warn_unused_result
  func f1() { }

  @warn_unused_result(message="huzzah")
  static func f2() { }

  @warn_unused_result
  func curried1()()() { }
}

func testMethodsNegative(c1: C1) {
  c1.f1 // expected-error{{expression resolves to an unused function}}
  c1.curried1() // expected-error{{expression resolves to an unused function}}
  c1.curried1()() // expected-error{{expression resolves to an unused function}}
}

func testMethodsPositive(c1: C1) {
  c1.f1() // expected-warning{{result of call to 'f1()' is unused}}
  C1.f2() // expected-warning{{result of call to 'f2()' is unused: huzzah}}
  c1.curried1()()() // expected-warning{{result of call to 'curried1()' is unused}}
}

struct Inits1 {
  @warn_unused_result init() { }
}

func testInitsPositive() {
  let _ = Inits1()
  Inits1() // expected-warning{{result of call to 'init()' is unused}}
}

// ---------------------------------------------------------------------------
// Warnings about unused results with mutating versions
// ---------------------------------------------------------------------------

struct Mutating1 {
  @warn_unused_result(mutable_variant="fooInPlace")
  func foo() -> Mutating1 { return self }

  mutating func fooInPlace() { }

  @warn_unused_result(message="zug zug", mutable_variant="barInPlace")
  func bar(x: Int, y: Int) -> Mutating1 { return self }

  mutating func barInPlace(x: Int, y: Int) { }
}

func testMutating1(m1: Mutating1, var m2: Mutating1) {
  m1.foo() // expected-warning{{result of call to 'foo()' is unused}}
  m2.foo() // expected-warning{{result of call to non-mutating function 'foo()' is unused; use 'fooInPlace()' to mutate in-place}}{{6-9=fooInPlace}}

  m1.bar(1, y: 1) // expected-warning{{result of call to 'bar(_:y:)' is unused: zug zug}}
  m2.bar(1, y: 1) // expected-warning{{result of call to non-mutating function 'bar(_:y:)' is unused; use 'barInPlace(_:y:)' to mutate in-place}}{{6-9=barInPlace}}
  m2 = m1
}

// ---------------------------------------------------------------------------
// Checking of the warn_unused_attribute itself
// ---------------------------------------------------------------------------
struct BadAttributes1 {
  @warn_unused_result(blarg) func f1() { } // expected-warning{{unknown parameter 'blarg' in 'warn_unused_result' attribute}}
  @warn_unused_result(wibble="foo") func f2() { } // expected-warning{{unknown parameter 'wibble' in 'warn_unused_result' attribute}}
  @warn_unused_result(message) func f3() { } // expected-error{{expected '=' following 'message' parameter}}
  @warn_unused_result(message=) func f4() { } // expected-error{{postfix '=' is reserved}}
  // expected-error@-1{{expected a string following '=' for 'message' parameter}}
  @warn_unused_result(message=blah) func f5() { } // expected-error{{expected a string following '=' for 'message' parameter}}

  @warn_unused_result(mutable_variant="oops") static func f6() { } // expected-error{{'mutable_variant' parameter of 'warn_unused_result' attribute does not make sense on a non-instance method}}
  @warn_unused_result(mutable_variant="oops") init() { } // expected-error{{'mutable_variant' parameter of 'warn_unused_result' attribute does not make sense on a non-function}}
  @warn_unused_result(mutable_variant="oops") mutating func f7() { } // expected-error{{'mutable_variant' parameter of 'warn_unused_result' attribute does not make sense on a mutating method}}
}

class BadAttributes2 {
  @warn_unused_result(mutable_variant="oops") func f1() { } // expected-error{{'mutable_variant' parameter of 'warn_unused_result' attribute does not make sense on a method of a class}}
}

@warn_unused_result(mutable_variant="oops") func badMutableVariant() { } // expected-error{{'mutable_variant' parameter of 'warn_unused_result' attribute does not make sense on a non-method}}
