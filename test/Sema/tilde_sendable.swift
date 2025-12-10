// RUN: %target-typecheck-verify-swift -strict-concurrency=complete -swift-version 5 -enable-experimental-feature TildeSendable -Wwarning ExplicitSendable

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

func testSendable<T: Sendable>(_: T) {}


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

  testSendable(NonSendable()) // expected-warning {{type 'NonSendable' does not conform to the 'Sendable' protocol}}
  testSendable(NoInference()) // Ok
}

func takesSendable<T: Sendable>(_: T) {}

class MyValue {} // expected-note 2 {{class 'MyValue' does not conform to the 'Sendable' protocol}}

public struct D: ~Sendable {
}

extension D: Sendable {} // expected-error {{cannot both conform to and suppress conformance to 'Sendable'}}

takesSendable(D())

public struct F<T>: ~Sendable {
  let x: T
}

extension F: Sendable where T: Sendable { }

takesSendable(F(x: 42))

public struct G<T, U>: ~Sendable { // expected-note {{making generic parameter 'U' conform to the 'Sendable' protocol}}
  let t: T
  let u: U // expected-warning {{stored property 'u' of 'Sendable'-conforming generic struct 'G' has non-Sendable type 'U'}}
}

extension G: Sendable where T: Sendable { }

takesSendable(G(t: "", u: 42))
takesSendable(G(t: MyValue(), u: 0)) // expected-warning {{type 'MyValue' does not conform to the 'Sendable' protocol}}

public struct H<T, U>: ~Sendable {
  let t: T
  let u: U
}

extension H: Sendable where T: Sendable, U: Sendable { }

takesSendable(H(t: "", u: 42))
takesSendable(H(t: "", u: MyValue())) // expected-warning {{type 'MyValue' does not conform to the 'Sendable' protocol}}

@MainActor
protocol IsolatedP {
}

@MainActor
protocol IsolatedSendableP: Sendable {
}

do {
  struct S1: ~Sendable, W {
    // expected-error@-1 {{cannot both conform to and suppress conformance to 'Sendable'}}
  }

  struct S2: IsolatedP, ~Sendable { // Ok (because isolated protocol is not Sendable unless explicitly stated)
  }

  struct S3: IsolatedSendableP, ~Sendable {
    // expected-error@-1 {{cannot both conform to and suppress conformance to 'Sendable'}}
  }

  @MainActor
  class IsolatedC: ~Sendable {}
  // expected-note@-1 {{class 'IsolatedC' explicitly suppresses conformance to 'Sendable' protocol}}

  testSendable(IsolatedC())
  // expected-warning@-1 {{type 'IsolatedC' does not conform to the 'Sendable' protocol}}

  @MainActor
  class IsolatedBase {} // derived as Sendable

  class Child1: IsolatedBase, ~Sendable {}
  // expected-error@-1 {{cannot both conform to and suppress conformance to 'Sendable'}}

  class Base: Sendable {}
  // expected-warning@-1 {{non-final class 'Base' cannot conform to the 'Sendable' protocol}}

  class Child2: Base, ~Sendable {}
  // expected-error@-1 {{cannot both conform to and suppress conformance to 'Sendable'}}

  actor A: ~Sendable {
    // expected-error@-1 {{conformance to 'Sendable' can only be suppressed on structs, classes, and enums}}
  }
}

// ExplicitSendable + ~Sendable tests

public struct TestExplicitSendable1 { // expected-warning {{public struct 'TestExplicitSendable1' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1 {{consider making struct 'TestExplicitSendable1' conform to the 'Sendable' protocol}}
  // expected-note@-2 {{consider suppressing conformance to 'Sendable' protocol}} {{36-36=: ~Sendable}}
  // expected-note@-3 {{make struct 'TestExplicitSendable1' explicitly non-Sendable to suppress this warning}}
}

public class TestExplicitSendableWithParent: ExpressibleByIntegerLiteral { // expected-warning {{public class 'TestExplicitSendableWithParent' does not specify whether it is 'Sendable' or not}}
  // expected-note@-1 {{consider suppressing conformance to 'Sendable' protocol}} {{73-73=, ~Sendable}}
  // expected-note@-2 {{make class 'TestExplicitSendableWithParent' explicitly non-Sendable to suppress this warning}}

  public required init(integerLiteral: Int) {
  }
}
