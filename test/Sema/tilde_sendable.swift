// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -swift-version 5 -enable-experimental-feature TildeSendable

// REQUIRES: swift_feature_TildeSendable

protocol P: ~Sendable { // expected-error {{conformance to 'Sendable' can only be suppressed on structs, classes, and enums}}
}

protocol Q {
  associatedtype T: ~Sendable
  // expected-error@-1 {{conformance to 'Sendable' can only be suppressed on structs, classes, and enums}}
}

struct A: ~Sendable {}
class B: ~Sendable {}
enum C: ~Sendable {}

struct E1: Sendable, ~Sendable {}
// expected-error@-1 {{cannot both conform to and suppress conformance to 'Sendable'}}

enum E2: ~Sendable, ~Sendable {} // expected-warning {{already suppressed conformance to 'Sendable'}}

struct InExt {}

extension InExt: ~Sendable { // expected-error {{conformance to inferrable protocol 'Sendable' cannot be suppressed in an extension}}
}

func test<T: ~Sendable>(_: T) {} // expected-error {{conformance to 'Sendable' can only be suppressed on structs, classes, and enums}}
func test<Q>(other: Q) where Q: ~Sendable {} // expected-error {{type 'Sendable' cannot be suppressed}}

struct Generic<T: ~Sendable> { // expected-error {{conformance to 'Sendable' can only be suppressed on structs, classes, and enums}}
  var x: T
}

var x: some BinaryInteger & ~Sendable // expected-error {{type 'Sendable' cannot be suppressed}}

protocol W: Sendable {
}

// Check that inference is suppressed by the annotation.
do {
  struct NonSendable: ~Sendable {
    // expected-note@-1 {{struct 'NonSendable' explicitly suppresses conformance to 'Sendable' protocol}}
    let x: Int = 0
  }

  struct NoInference: W, ~Sendable {
    // expected-error@-1 {{cannot both conform to and suppress conformance to 'Sendable'}}
  }

  func check<T: Sendable>(_: T) {}

  check(NonSendable()) // expected-warning {{type 'NonSendable' does not conform to the 'Sendable' protocol}}
  check(NoInference()) // Ok
}
