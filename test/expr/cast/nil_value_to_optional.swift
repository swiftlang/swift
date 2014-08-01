// RUN: %swift -parse %s -verify

var t = true
var f = false
println(t != nil) // expected-error{{could not find an overload for 'println' that accepts the supplied arguments}}
println(f != nil) // expected-error{{could not find an overload for 'println' that accepts the supplied arguments}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(c: C) {
  if c == nil {} // expected-error{{type 'C' does not conform to protocol 'NilLiteralConvertible'}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil

d == nil // expected-error{{cannot invoke '==' with an argument list of type '(@lvalue D, NilLiteralConvertible)'}}
dopt == nil
diuopt == nil
