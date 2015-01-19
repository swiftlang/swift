// RUN: %target-parse-verify-swift

var t = true
var f = false
println(t != nil) // expected-error{{cannot find an overload for 'println' that accepts an argument list of type '(Bool)'}}
println(f != nil) // expected-error{{cannot find an overload for 'println' that accepts an argument list of type '(Bool)'}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(c: C) {
  if c == nil {} // expected-error{{binary operator '==' cannot be applied operands of type 'C' and 'nil'}} expected-note{{Overloads for '==' exist with these partially matching parameter lists: (C, C)}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil

d == nil // expected-error{{binary operator '==' cannot be applied operands of type 'D' and 'nil'}}
dopt == nil
diuopt == nil
