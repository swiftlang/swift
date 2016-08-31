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
func f1(_ x: Fooable & Barable) {}
func f2(_: Float) {}
let nilFunc: Optional<(Barable) -> ()> = nil

func g(_: (Barable & Fooable) -> ()) {}

protocol Classable : AnyObject {}
class SomeArbitraryClass {}

func fc0(_: Classable) {}
func fc1(_: Fooable & Classable) {}
func fc2(_: AnyObject) {}
func fc3(_: SomeArbitraryClass) {}
func gc(_: (Classable & Fooable) -> ()) {}

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

f1(f) // expected-error{{argument type 'Float' does not conform to expected type 'Barable & Fooable'}}
f1(b) // expected-error{{argument type 'Barable' does not conform to expected type 'Barable & Fooable'}}

//===----------------------------------------------------------------------===//
// Subtyping
//===----------------------------------------------------------------------===//
g(f0) // okay (subtype)
g(f1) // okay (exact match)

g(f2) // expected-error{{cannot convert value of type '(Float) -> ()' to expected argument type '(Barable & Fooable) -> ()'}}

// FIXME: Workaround for ?? not playing nice with function types.
infix operator ??*
func ??*<T>(lhs: T?, rhs: T) -> T { return lhs ?? rhs }
g(nilFunc ??* f0)

gc(fc0) // okay
gc(fc1) // okay
gc(fc2) // okay
gc(fc3) // expected-error{{cannot convert value of type '(SomeArbitraryClass) -> ()' to expected argument type '(Classable & Fooable) -> ()'}}

// rdar://problem/19600325
func getAnyObject() -> AnyObject? {
  return SomeArbitraryClass()
}

func castToClass(_ object: Any) -> SomeArbitraryClass? {
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
  func doubleMaybeClone() -> Self??
  func subdivideClone() -> (Self, Self)
  func metatypeOfClone() -> Self.Type
  func badClonerFn() -> ((Self) -> Self)
  func veryBadClonerFn() -> ((inout Self) -> ())
  func goodClonerFn() -> (() -> Self)
}

func testClonable(_ v : Clonable) { // expected-error {{protocol 'Clonable' can only be used as a generic constraint because it has Self or associated type requirements}}
  let v2 = v.maybeClone()
  let v3 = v.doubleMaybeClone()
  let v4 = v.subdivideClone()
  let v5 = v.metatypeOfClone()
  let v6 = v.goodClonerFn()

  let v7 = v.badClonerFn() // expected-error {{member 'badClonerFn' cannot be used on value of protocol type 'Clonable'; use a generic constraint instead}}
  let v8 = v.veryBadClonerFn() // expected-error {{member 'veryBadClonerFn' cannot be used on value of protocol type 'Clonable'; use a generic constraint instead}}
}
