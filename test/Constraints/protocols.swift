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
let nilFunc: Optional<(Barable) -> ()> = nil

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

//===----------------------------------------------------------------------===//
// Conversion to and among existential types
//===----------------------------------------------------------------------===//

f0(i)
f0(f)
f0(b)
f1(i)

f1(f) // expected-error{{argument type 'Float' does not conform to expected type 'protocol<Barable, Fooable>'}}
f1(b) // expected-error{{argument type 'Barable' does not conform to expected type 'protocol<Barable, Fooable>'}}

//===----------------------------------------------------------------------===//
// Subtyping
//===----------------------------------------------------------------------===//
g(f0) // okay (subtype)
g(f1) // okay (exact match)

g(f2) // expected-error{{cannot convert value of type '(Float) -> ()' to expected argument type '(protocol<Barable, Fooable>) -> ()'}}

// FIXME: Workaround for ?? not playing nice with function types.
infix operator ??* {}
func ??*<T>(lhs: T?, rhs: T) -> T { return lhs ?? rhs }
g(nilFunc ??* f0)

gc(fc0) // okay
gc(fc1) // okay
gc(fc2) // okay
gc(fc3) // expected-error{{cannot convert value of type '(SomeArbitraryClass) -> ()' to expected argument type '(protocol<Classable, Fooable>) -> ()'}}

// rdar://problem/19600325
func getAnyObject() -> AnyObject? {
  return SomeArbitraryClass()
}

func castToClass(object: Any) -> SomeArbitraryClass? {
  return object as? SomeArbitraryClass
}

_ = getAnyObject().map(castToClass)


_ = { (_: Any) -> Void in
  return
} as ((Int) -> Void)

let _: (Int) -> Void = {
  (_: Any) -> Void in
  return
}

let _: () -> Any = {
  () -> Int in
  return 0
}

let _: () -> Int = { 
  () -> String in  // expected-error {{declared closure result 'String' is incompatible with contextual type 'Int'}}
  return ""
}

//===----------------------------------------------------------------------===//
// Dynamic self
//===----------------------------------------------------------------------===//
protocol Clonable {
  func maybeClone() -> Self?
  func badMaybeClone() -> Self??
}

func testClonable(v : Clonable) {
  let v2 = v.maybeClone()

  let v3 = v.badMaybeClone() // expected-error {{member 'badMaybeClone' cannot be used on value of protocol type 'Clonable'; use a generic constraint instead}}
}
