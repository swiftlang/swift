// RUN: %swift %s -verify

// FIXME: Not currently testing variadic arguments.
class A {
  constructor (initWithInt : Int, bar : Double) { }

  func foo(i : Int) bar(d : Double) { }
  func foo(i : Int) bar(d : Double) wibble(s: String) { }
}

func testWellFormed(a : A) {
  a.foo(42) bar(3.14159)
  a.foo(42)
   :bar(3.14159)
}

func testIllformed1(a : A) {
  a.foo(42) bar(3.14159) :wibble("hello") // expected-error{{consecutive statements on a line must be separated by ';'}} expected-error{{expected expression}}
}

func testIllformed2(a : A) {
  a.foo(42) bar (3.14159) // expected-error{{cannot have whitespace prior to '(' in a call}}
}

func testIllformed3(a : A) {
  a.foo(42) bar(bar=3.14159) // expected-error{{selector call argument cannot be named}}
}

func testIllformed4(a : A) {
  a.foo(42) bar(bar=3.14159) // expected-error{{selector call argument cannot be named}}
}

func testIllformed5(a : A) {
  a.foo(42) bar(3.14159, "hello") // expected-error{{call argument can have at most one argument (2 provided)}}
}

// test that we expose all errors in the args
func testIllFormed6(a : A) {
  a.foo(0xQWERTY) // expected-error{{expected a digit after integer literal prefix}}
   :bar(0xZXCVBNM) // expected-error{{expected a digit after integer literal prefix}}
}

// Super sends with selector arguments
class B : A {
  constructor (initWithInt : Int, bar : Double) { 
    super.constructor(initWithInt=initWithInt) bar(bar)
  }

  func test(i : Int, d : Double) {
    super.foo(i) bar(d)
  }
}

// New expressions with selector arguments
func testNew() {
  new A(initWithInt=1) bar(3.14159)
}

