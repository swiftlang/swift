// RUN: %target-parse-verify-swift

// Deduction of associated types.
protocol Fooable {
  typealias AssocType
  func foo(x : AssocType)
}

struct X : Fooable {
  func foo(x: Float) {}
}

struct Y<T> : Fooable {
  func foo(x: T) {}
}

struct Z : Fooable {
  func foo(x: Float) {}

  // FIXME: Feels inconsistent that we can't refer to AssocType
  // unqualified, but qualified is okay.

  func blah() {
    var at : AssocType // expected-error{{use of undeclared type 'AssocType'}}
  }

  func blarg() -> AssocType {} // expected-error{{use of undeclared type 'AssocType'}}

  func wonka() -> Z.AssocType {}
}

var xa : X.AssocType = Float()
var yf : Y<Float>.AssocType = Float()
var yd : Y<Double>.AssocType = Double()

var f : Float
f = xa
f = yf

var d : Double
d = yd

protocol P1 {
  typealias Assoc1
  func foo() -> Assoc1
}

struct S1 : P1 {
  func foo() -> X {}
}

prefix operator % {}

protocol P2 {
  typealias Assoc2
  prefix func %(target: Self) -> Assoc2
}

prefix func % <P:P1>(target: P) -> P.Assoc1 {
}

extension S1 : P2 {
  typealias Assoc2 = X
}

// <rdar://problem/14418181>
protocol P3 {
  typealias Assoc3
  func foo() -> Assoc3
}

protocol P4 : P3 {
  typealias Assoc4
  func bar() -> Assoc4
}

func takeP4<T : P4>(x: T) { }

struct S4<T> : P3, P4 {
  func foo() -> Int {}
  func bar() -> Double {}
}

takeP4(S4<Int>())

// <rdar://problem/14680393>
infix operator ~> { precedence 255 }

protocol P5 { }

struct S7a {}

protocol P6 {
  func foo<Target: P5>(inout target: Target)
}

protocol P7 : P6 {
  typealias Assoc : P6
  func ~> (x: Self, _: S7a) -> Assoc
}

func ~> <T:P6>(x: T, _: S7a) -> S7b { return S7b() }

struct S7b : P7 {
  func foo<Target: P5>(inout target: Target) {}
}

// <rdar://problem/14685674>
struct zip<A: GeneratorType, B: GeneratorType> : GeneratorType, SequenceType {
     func next() -> (A.Element, B.Element)? { }

     typealias Generator = zip
     func generate() -> zip { }
     
}

protocol P8 { }

protocol P9 {
  typealias A1 : P8
}

protocol P10 {
  typealias A1b : P8
  typealias A2 : P9

  func f()
  func g(a: A1b)
  func h(a: A2)
}

struct X8 : P8 { }

struct Y9 : P9 {
  typealias A1 = X8
}

struct Z10 : P10 {
  func f() { }
  func g(a: X8) { }
  func h(a: Y9) { }
}


struct W : Fooable {
  func foo(x: String) {}
}
struct V<T> : Fooable {
  func foo(x: T) {}
}

// FIXME: <rdar://problem/16123805> Inferred associated types can't be used in expression contexts
var w = W.AssocType() // expected-error{{'W.Type' does not have a member named 'AssocType'}}
var v = V<String>.AssocType() // expected-error{{'V<String>.Type' does not have a member named 'AssocType'}}
