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

protocol Classable : AnyObject {}
class SomeArbitraryClass {}

func fc0(_: Classable) {}
func fc1(_: protocol<Fooable, Classable>) {}
func fc2(_: AnyObject) {}
func fc3(_: SomeArbitraryClass) {}
func gc(_: (protocol<Classable, Fooable>) -> ()) {}

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
g(f0) // expected-error{{cannot invoke 'g' with an argument list of type '((Barable) -> ())'}} expected-note{{expected an argument list of type '((protocol<Barable, Fooable>) -> ())'}}
g(f1) // okay (exact match)

g(f2) // expected-error{{cannot invoke 'g' with an argument list of type '((Float) -> ())'}} expected-note{{expected an argument list of type '((protocol<Barable, Fooable>) -> ())'}}

gc(fc0) // okay
gc(fc1) // okay
gc(fc2) // okay
gc(fc3) // expected-error{{cannot invoke 'gc' with an argument list of type '((SomeArbitraryClass) -> ())'}} expected-note{{expected an argument list of type '((protocol<Classable, Fooable>) -> ())'}}

// rdar://problem/19600325
func getAnyObject() -> AnyObject? {
  return SomeArbitraryClass()
}

func castToClass(object: Any) -> SomeArbitraryClass? {
  return object as? SomeArbitraryClass
}

getAnyObject().map(castToClass) // expected-error{{cannot invoke 'map' with an argument list of type '((Any) -> SomeArbitraryClass?)'}}


let _ = { (_: Any) -> Void in
  return
} as ((Int) -> Void) // expected-error {{'Any -> Void' is not convertible to '(Int) -> Void'}}

let _: (Int) -> Void = {  // expected-error {{'Int' is not a subtype of 'Any'}}
	(_: Any) -> Void in
	return
}

let _: () -> Any = {  // expected-error {{'Int' is not a subtype of 'Any'}}
	() -> Int in
	return 0
}

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
