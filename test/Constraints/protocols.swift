// RUN: %swift -parse -verify %s

protocol Fooable { func foo() }
protocol Barable { func bar() }

extension Int : Fooable, Barable {
  func foo() {}
  func bar() {}
}

extension Float32 : Barable {
  func bar() {}
}

func f0(_: Barable) {}
func f1(x: protocol<Fooable, Barable>) {} // expected-note 2{{in initialization of parameter 'x'}}
func f2(_: Float) {}

func g(_: (protocol<Barable, Fooable>) -> ()) {} // expected-note{{in call to function 'g'}}

var i : Int
var f : Float
var b : Barable

//===--------------------------------------------------------------------===//
// Conversion to and among existential types
//===--------------------------------------------------------------------===//

f0(i)
f0(f)
f0(b)
f1(i)

f1(f) // expected-error{{type 'Float' does not conform to protocol 'Fooable'}}
f1(b) // expected-error{{type 'Barable' does not conform to protocol 'Fooable'}}

//===--------------------------------------------------------------------===//
// Subtyping
//===--------------------------------------------------------------------===//
g(f0)
g(f1)

g(f2) // expected-error{{'protocol<Barable, Fooable>' is not a subtype of 'Float'}}

//===--------------------------------------------------------------------===//
// User-defined conversions
//===--------------------------------------------------------------------===//
struct X { }

protocol P {
  @conversion func __conversion() -> Q
}

protocol Q { }

func testUserConvert(p: P) {
  var q : Q = p
}

func myPrintf(format: String, args: Any...) {}
myPrintf("hello", i, f, 3.14159)

// FIXME: Customize diagnostic
Fooable() // expected-error{{'Fooable' is not constructible with '()'}}

protocol P2 { }

class Y : P2 { }

class Z {
  @conversion func __conversion () -> Y { return Y() }
}

var x: P2 = Z()

//===--------------------------------------------------------------------===//
// Dynamic self
//===--------------------------------------------------------------------===//
protocol Clonable {
  func maybeClone() -> Self?
  func badMaybeClone() -> Self??
}

func testClonable(v : Clonable) {
  let v2 = v.maybeClone()

  // FIXME: this is a terrible diagnostic; the problem is that that
  // method is unavailable on existentials
  let v3 = v.badMaybeClone() // expected-error {{'Clonable' does not have a member named 'badMaybeClone'}}
}
