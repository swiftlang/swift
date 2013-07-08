// RUN: %swift -parse %s -verify

// Deduction of associated types.
protocol Fooable {
  typealias AssocType
  func foo(x : AssocType)
}

struct X : Fooable {
  func foo(x : Float) {}
}

struct Y<T> : Fooable {
  func foo(x : T) {}
}

struct Z : Fooable {
  func foo(x : Float) {}

  func blah() {
    var at : AssocType // expected-error{{use of undeclared type 'AssocType'}}
  }

  func blarg() -> AssocType {} // expected-error{{use of undeclared type 'AssocType'}}

  func wonka() -> Z.AssocType {} // expected-error{{'AssocType' is not a member type of 'Z'}}
}

var xa : X.AssocType = Float()
var yf : Y<Float>.AssocType = Float()
var yd : Y<Double>.AssocType = Double()

var f : Float
f = xa
f = yf

var d : Double
d = yd

