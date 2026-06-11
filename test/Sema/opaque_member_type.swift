// RUN: %target-swift-frontend -typecheck -verify %s

protocol P {
  associatedtype T
  var t: T { get }
}

struct S: P {
  var t: Int { 23 }
}

let spt: (some P).T = S().t // expected-error {{cannot access member type}}

struct S2<T> {}

protocol Q {}

extension Q {
  typealias X = S2<Self>
}

protocol R {
  associatedtype X
}

// These are fine, `some Q` is implicitly a generic parameter.
func asParameter1(_: (some Q).X) {}
func asParameter2(_: [(some Q).X]) {}

// This is not fine, there is no way to infer the generic parameter.
// FIXME: We ought to have a better diagnostic for this.
func asParameter3(_: (some R).X) {} // expected-error {{generic parameter '_' is not used in function signature}}
