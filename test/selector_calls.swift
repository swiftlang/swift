// RUN: %swift %s -verify

// FIXME: Not currently testing variadic arguments.
class A {
  func foo(i : Int) bar(d : Double) { }
}

func testWellFormed(a : A) {
  a.(foo:42 bar:3.14159)
  a.foo(42, bar:3.14159) // equivalent to the above
}

func testIllFormed1(a : A) {
  a.(); // expected-error{{expected identifier naming the next argument}}
}

func testIllFormed2(a : A) {
  a.(42) // expected-error{{expected identifier naming the next argument}}
}

func testIllFormed3(a : A) {
  a.(foo=42) // expected-error{{expected ':' to separate the argument name ('foo') and the argument}}
}

func testIllFormed4(a : A) {
  a.(foo:) // expected-error{{expected argument expression}}
}
func testIllFormed5(a : A) {
  a.(foo:42, // expected-note{{to match this opening '('}}
     bar:3.14159) // expected-error{{expected ')' to close message argument list}}
}

class B : A {
  func test(i : Int, d : Double) {
    super.(foo:i bar:d)
  }
}
