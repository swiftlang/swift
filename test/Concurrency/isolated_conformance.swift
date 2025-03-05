// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -enable-experimental-feature IsolatedConformances %s

// REQUIRES: swift_feature_IsolatedConformances

protocol P {
  func f() // expected-note 2{{mark the protocol requirement 'f()' 'async' to allow actor-isolated conformances}}
}

// ----------------------------------------------------------------------------
// Definition of isolated conformances
// ----------------------------------------------------------------------------

// expected-note@+3{{add '@preconcurrency' to the 'P' conformance to defer isolation checking to run time}}{{25-25=@preconcurrency }}
// expected-note@+2{{add 'isolated' to the 'P' conformance to restrict it to main actor-isolated code}}{{25-25=isolated }}
@MainActor
class CWithNonIsolated: P {
  func f() { } // expected-error{{main actor-isolated instance method 'f()' cannot be used to satisfy nonisolated requirement from protocol 'P'}}
  // expected-note@-1{{add 'nonisolated' to 'f()' to make this instance method not isolated to the actor}}
}

actor SomeActor { }

// Isolated conformances need a global-actor-constrained type.
class CNonIsolated: isolated P { // expected-error{{isolated conformance is only permitted on global-actor-isolated types}}
  func f() { }
}

extension SomeActor: isolated P { // expected-error{{isolated conformance is only permitted on global-actor-isolated types}}
  nonisolated func f() { }
}

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

// Isolation of the function needs to match that of the enclosing type.
@MainActor
class CMismatchedIsolation: isolated P {
  @SomeGlobalActor func f() { } // expected-error{{global actor 'SomeGlobalActor'-isolated instance method 'f()' cannot be used to satisfy nonisolated requirement from protocol 'P'}}
}

@MainActor
class C: isolated P {
  func f() { } // okay
}

// Associated conformances with isolation

protocol Q {
  associatedtype A: P
}

// expected-error@+2{{conformance of 'SMissingIsolation' to 'Q' depends on main actor-isolated conformance of 'C' to 'P'; mark it as 'isolated'}}{{27-27=isolated }}
@MainActor
struct SMissingIsolation: Q {
  typealias A = C
}

struct PWrapper<T: P>: P {
  func f() { }
}

// expected-error@+2{{conformance of 'SMissingIsolationViaWrapper' to 'Q' depends on main actor-isolated conformance of 'C' to 'P'; mark it as 'isolated'}}
@MainActor
struct SMissingIsolationViaWrapper: Q {
  typealias A = PWrapper<C>
}

@SomeGlobalActor
class C2: isolated P {
  func f() { }
}

@MainActor
struct S: isolated Q {
  typealias A = C
}

// expected-error@+2{{main actor-isolated conformance of 'SMismatchedActors' to 'Q' cannot depend on global actor 'SomeGlobalActor'-isolated conformance of 'C2' to 'P'}}
@MainActor
struct SMismatchedActors: isolated Q {
  typealias A = C2
}

// ----------------------------------------------------------------------------
// Use checking of isolated conformances.
// ----------------------------------------------------------------------------

// expected-note@+1{{requirement specified as 'T' : 'P' [with T = C]}}
struct PSendableWrapper<T: P & Sendable>: P {
  func f() { }
}

// expected-note@+1{{requirement specified as 'T' : 'P' [with T = C]}}
struct PSendableMetaWrapper<T: P & SendableMetatype>: P {
  func f() { }
}

@MainActor
func testIsolationConformancesInTypes() {
  typealias A1 = PWrapper<C>
  typealias A2 = PSendableWrapper<C> // expected-error{{isolated conformance of 'C' to 'P' cannot be used to satisfy conformance requirement for a `Sendable` type parameter 'T'}}
  typealias A3 = PSendableMetaWrapper<C> // expected-error{{isolated conformance of 'C' to 'P' cannot be used to satisfy conformance requirement for a `SendableMetatype` type parameter 'T'}}
}

func acceptP<T: P>(_: T) { }

func acceptSendableP<T: Sendable & P>(_: T) { }
// expected-note@-1{{'acceptSendableP' declared here}}

func acceptSendableMetaP<T: SendableMetatype & P>(_: T) { }
// expected-note@-1{{'acceptSendableMetaP' declared here}}

@MainActor
func testIsolationConformancesInCall(c: C) {
  acceptP(c) // okay

  acceptSendableP(c) // expected-error{{isolated conformance of 'C' to 'P' cannot be used to satisfy conformance requirement for a `Sendable` type parameter}}
  acceptSendableMetaP(c) // expected-error{{isolated conformance of 'C' to 'P' cannot be used to satisfy conformance requirement for a `Sendable` type parameter}}
}

func testIsolationConformancesFromOutside(c: C) {
  acceptP(c) // expected-error{{main actor-isolated isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  let _: any P = c // expected-error{{main actor-isolated isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  let _ = PWrapper<C>() // expected-error{{main actor-isolated isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
}
