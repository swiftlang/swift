// RUN: %target-parse-verify-swift

var t = true
var f = false

func markUsed<T>(_ t: T) {}

markUsed(t != nil) // expected-error {{value of type 'Bool' can never be nil, comparison isn't allowed}}
markUsed(f != nil) // expected-error {{value of type 'Bool' can never be nil, comparison isn't allowed}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(_ c: C) {
  if c == nil {} // expected-error {{value of type 'C' can never be nil, comparison isn't allowed}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil

_ = d == nil // expected-error{{value of type 'D' can never be nil, comparison isn't allowed}}
_ = dopt == nil
_ = diuopt == nil
