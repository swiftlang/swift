// RUN: %target-swift-frontend -typecheck -verify %s

protocol P {
  associatedtype T
  var t: T { get }
}

struct S: P {
  var t: Int { 23 }
}

let spt: (some P).T = S().t // expected-error {{cannot access member type}}
