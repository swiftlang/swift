// RUN: %target-parse-verify-swift

var t = true
var f = false

func markUsed<T>(_ t: T) {}

markUsed(t != nil) // expected-error {{type 'Bool' is not optional, value can never be nil}}
markUsed(f != nil) // expected-error {{type 'Bool' is not optional, value can never be nil}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(_ c: C) {
  if c == nil {} // expected-error {{type 'C' is not optional, value can never be nil}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil

_ = d == nil // expected-error{{type 'D' is not optional, value can never be nil}}
_ = dopt == nil
_ = diuopt == nil
