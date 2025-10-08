// RUN: %target-typecheck-verify-swift -enable-experimental-feature TildeSendable

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

