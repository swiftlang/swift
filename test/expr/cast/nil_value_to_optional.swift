// RUN: %target-typecheck-verify-swift

var t = true
var f = false

func markUsed<T>(_ t: T) {}

markUsed(t != nil) // expected-warning {{comparing non-optional value of type 'Bool' to 'nil' always returns true}}
markUsed(f != nil) // expected-warning {{comparing non-optional value of type 'Bool' to 'nil' always returns true}}
markUsed(t != Optional.none) // expected-warning {{comparing non-optional value of type 'Bool' to 'Optional.none' always returns true}}
markUsed(f != Optional.none) // expected-warning {{comparing non-optional value of type 'Bool' to 'Optional.none' always returns true}}

class C : Equatable {}

func == (lhs: C, rhs: C) -> Bool {
  return true
}

func test(_ c: C) {
  if c == nil {}  // expected-warning {{comparing non-optional value of type 'C' to 'nil' always returns false}}
  if c == .none {}  // expected-warning {{comparing non-optional value of type 'C' to 'Optional.none' always returns false}}
  if c == Optional.none {}  // expected-warning {{comparing non-optional value of type 'C' to 'Optional.none' always returns false}}
  if c == C?.none {}  // expected-warning {{comparing non-optional value of type 'C' to 'Optional.none' always returns false}}
}

class D {}

var d = D()
var dopt: D? = nil
var diuopt: D! = nil
func produceD() -> D! { D() }

_ = d! // expected-error {{cannot force unwrap value of non-optional type 'D'}}
_ = dopt == nil
_ = diuopt == nil
_ = diuopt is ExpressibleByNilLiteral // expected-warning {{'is' test is always true}}
_ = produceD() is ExpressibleByNilLiteral // expected-warning {{'is' test is always true}}

enum E {
  case none
}
func test(_ e: E) {
  _ = e == .none
  _ = e == E.none
  _ = e == Optional.none // expected-warning {{comparing non-optional value of type 'E' to 'Optional.none' always returns false}}
  _ = e == E?.none // expected-warning {{comparing non-optional value of type 'E' to 'Optional.none' always returns false}}
}
