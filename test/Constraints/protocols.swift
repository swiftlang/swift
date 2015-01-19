// RUN: %target-parse-verify-swift

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
func f1(x: protocol<Fooable, Barable>) {}
func f2(_: Float) {}

func g(_: (protocol<Barable, Fooable>) -> ()) {}

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

f1(f) // expected-error{{cannot invoke 'f1' with an argument list of type '(Float)'}} expected-note{{expected an argument list of type '(protocol<Barable, Fooable>)'}}
f1(b) // expected-error{{cannot invoke 'f1' with an argument list of type '(Barable)'}} expected-note{{expected an argument list of type '(protocol<Barable, Fooable>)'}}

//===--------------------------------------------------------------------===//
// Subtyping
//===--------------------------------------------------------------------===//
g(f0)
g(f1)

g(f2) // expected-error{{cannot invoke 'g' with an argument list of type '((Float) -> ())'}} expected-note{{expected an argument list of type '((protocol<Barable, Fooable>) -> ())'}}

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
