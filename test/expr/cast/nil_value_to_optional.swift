// RUN: %swift -parse %s -verify

var t = true
var f = false
println(t != nil) // expected-error{{binary operator '!=' cannot be applied to an Bool operand and a nil operand}}
println(f != nil) // expected-error{{binary operator '!=' cannot be applied to an Bool operand and a nil operand}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(c: C) {
  if c == nil {} // expected-error{{binary operator '==' cannot be applied to an C operand and a nil operand}} expected-note{{Overloads for '==' exist with these partially matching parameter lists:}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil

d == nil // expected-error{{binary operator '==' cannot be applied to an D operand and a nil operand}}
dopt == nil
diuopt == nil
