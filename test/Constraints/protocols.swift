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
g(f0) // expected-error{{function signature '(Barable) -> ()' is not compatible with expected type '(protocol<Barable, Fooable>) -> ()'}} expected-note{{use a closure to safely wrap calls to the function}} {{3-3={ }} {{5-5=($0) }}}
g(f1) // okay (exact match)

g(f2) // expected-error{{cannot convert value of type '(Float) -> ()' to expected argument type '(protocol<Barable, Fooable>) -> ()'}}

// FIXME: Workaround for ?? not playing nice with function types.
infix operator ??* {}
func ??*<T>(lhs: T?, rhs: T) -> T { return lhs ?? rhs }
g(nilFunc ??* f0) // expected-error {{function signature '(Barable) -> ()' is not compatible with expected type '(protocol<Barable, Fooable>) -> ()'}} expected-note {{use a closure to safely wrap calls to the function}} {{3-3={ (}} {{17-17=)($0) }}}

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

_ = getAnyObject().map(castToClass) // expected-error{{function signature '(Any) -> SomeArbitraryClass?' is not compatible with expected type '(AnyObject) throws -> SomeArbitraryClass?'}} expected-note {{use a closure to safely wrap calls to the function}} {{24-24={ }} {{35-35=($0) }}}


_ = { (_: Any) -> Void in // expected-error {{function signature 'Any -> Void' is not compatible with expected type '(Int) -> Void'}}
  return
} as ((Int) -> Void)

let _: (Int) -> Void = {  // expected-error {{function signature 'Any -> Void' is not compatible with expected type '(Int) -> Void'}}
  (_: Any) -> Void in
  return
}

let _: () -> Any = {  // expected-error {{function signature '() -> Int' is not compatible with expected type '() -> Any'}}
  () -> Int in
  return 0
}

let _: () -> Int = { // expected-error {{cannot convert value of type '() -> String' to specified type '() -> Int'}}
  () -> String in
  return ""
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

  let v3 = v.badMaybeClone() // expected-error {{member 'badMaybeClone' cannot be used on value of protocol type 'Clonable'; use a generic constraint instead}}
}
