// RUN: %swift -parse %s -verify

var t = true
var f = false
println(t != nil) // expected-error{{'Bool' is not convertible to 'UInt8'}}
println(f != nil) // expected-error{{'Bool' is not convertible to 'UInt8'}}

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

d == nil // expected-error{{'D' is not convertible to 'C'}}
dopt == nil
diuopt == nil
