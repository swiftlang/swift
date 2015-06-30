// RUN: %target-parse-verify-swift

var t = true
var f = false

func markUsed<T>(t: T) {}

markUsed(t != nil) // expected-error {{binary operator '!=' cannot be applied to operands of type 'Bool' and 'nil'}}
markUsed(f != nil) // expected-error {{binary operator '!=' cannot be applied to operands of type 'Bool' and 'nil'}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(c: C) {
  if c == nil {} // expected-error {{binary operator '==' cannot be applied to operands of type 'C' and 'nil'}} expected-note {{overloads for '==' exist with these partially matching parameter lists: (C, C)}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil

d == nil // expected-error{{binary operator '==' cannot be applied to operands of type 'D' and 'nil'}}
dopt == nil
diuopt == nil
