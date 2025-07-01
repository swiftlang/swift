// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 5 -strict-concurrency=complete %s

// REQUIRES: concurrency

protocol P {
  func f()
}

// ----------------------------------------------------------------------------
// Definition of isolated conformances
// ----------------------------------------------------------------------------

// expected-warning@+4{{conformance of 'CWithNonIsolated' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
// expected-note@+3{{mark all declarations used in the conformance 'nonisolated'}}
// expected-note@+2{{isolate this conformance to the main actor with '@MainActor'}}{{25-25=@MainActor }}
@MainActor
class CWithNonIsolated: P {
  // expected-note@-1{{turn data races into runtime errors with '@preconcurrency'}}
  func f() { } // expected-note{{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
}

actor SomeActor { }

// Isolated conformances need a global-actor-constrained type.
class CNonIsolated: @MainActor P {
  func f() { }
}

extension SomeActor: @MainActor P {
  @MainActor func f() { }
}

@globalActor
struct SomeGlobalActor {
  static let shared = SomeActor()
}

// Isolation of the conformance can be different from that of the enclosing
// type, so long as the witnesses match up.
@MainActor
class CMismatchedIsolation: @SomeGlobalActor P {
  @SomeGlobalActor func f() { }
}

@MainActor
class C: @MainActor P {
  func f() { } // okay
}

// Associated conformances with isolation

protocol Q {
  associatedtype A: P
}

// expected-warning@+2{{conformance of 'SMissingIsolation' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
@MainActor
struct SMissingIsolation: Q {
  // expected-note@-1{{conformance depends on main actor-isolated conformance of 'C' to protocol 'P'}}
  // expected-note@-2{{isolate this conformance to the main actor with '@MainActor'}}
  typealias A = C
}

struct PWrapper<T: P>: P {
  func f() { }
}

// expected-warning@+2{{conformance of 'SMissingIsolationViaWrapper' to protocol 'Q' crosses into main actor-isolated code and can cause data races}}
@MainActor
struct SMissingIsolationViaWrapper: Q {
  // expected-note@-1{{conformance depends on main actor-isolated conformance of 'C' to protocol 'P'}}
  // expected-note@-2{{isolate this conformance to the main actor with '@MainActor'}}{{37-37=@MainActor }}
  typealias A = PWrapper<C>
}

@SomeGlobalActor
class C2: @SomeGlobalActor P {
  func f() { }
}

@MainActor
struct S: @MainActor Q {
  typealias A = C
}

// expected-warning@+3{{conformance of 'SMismatchedActors' to protocol 'Q' crosses into global actor 'SomeGlobalActor'-isolated code and can cause data races}}
// expected-note@+2{{conformance depends on global actor 'SomeGlobalActor'-isolated conformance of 'C2' to protocol 'P'}}
@MainActor
struct SMismatchedActors: @MainActor Q {
  typealias A = C2
}

protocol PSendable: P, Sendable { }

// expected-error@+2{{type 'PSendableS' does not conform to protocol 'PSendable'}}
// expected-error@+1{{main actor-isolated conformance of 'PSendableS' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'Self'}}
struct PSendableS: @MainActor PSendable { // expected-note{{requirement specified as 'Self' : 'P' [with Self = PSendableS]}}
  func f() { }
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
  typealias A2 = PSendableWrapper<C> // expected-error{{isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter 'T'}}
  typealias A3 = PSendableMetaWrapper<C> // expected-error{{isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'SendableMetatype' type parameter 'T'}}
}

func acceptP<T: P>(_: T) { }

func acceptSendableP<T: Sendable & P>(_: T) { }
// expected-note@-1{{'acceptSendableP' declared here}}

func acceptSendableMetaP<T: SendableMetatype & P>(_: T) { }
// expected-note@-1 3{{'acceptSendableMetaP' declared here}}

@MainActor
func testIsolationConformancesInCall(c: C) {
  acceptP(c) // okay

  acceptSendableP(c) // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
  acceptSendableMetaP(c) // expected-error{{isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

@MainActor
func testIsolatedConformancesOfActor(a: SomeActor) {
  acceptP(a)
  acceptSendableMetaP(a) // expected-error{{main actor-isolated conformance of 'SomeActor' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

@SomeGlobalActor
func testIsolatedConformancesOfOtherGlobalActor(c: CMismatchedIsolation) {
  acceptP(c)
  acceptSendableMetaP(c)  // expected-error{{global actor 'SomeGlobalActor'-isolated conformance of 'CMismatchedIsolation' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}
}

func testIsolationConformancesFromOutside(c: C) {
  acceptP(c) // expected-warning{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  let _: any P = c // expected-warning{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
  let _ = PWrapper<C>() // expected-warning{{main actor-isolated conformance of 'C' to 'P' cannot be used in nonisolated context}}
}

protocol HasAssociatedType {
  associatedtype A
}

func acceptHasAssocWithP<T: HasAssociatedType>(_: T) where T.A: P { }

func acceptSendableHasAssocWithP<T: Sendable & HasAssociatedType>(_: T) where T.A: P { }
// expected-note@-1{{'acceptSendableHasAssocWithP' declared here}}


struct HoldsC: HasAssociatedType {
  typealias A = C
}

extension HasAssociatedType {
  static func acceptAliased<T: P>(_: T.Type) where A == T { }
}

extension HasAssociatedType where Self: Sendable {
  static func acceptSendableAliased<T: P>(_: T.Type) where A == T { }
}

func testIsolatedConformancesOnAssociatedTypes(hc: HoldsC, c: C) {
  acceptHasAssocWithP(hc)
  acceptSendableHasAssocWithP(hc) // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter }}

  HoldsC.acceptAliased(C.self) // okay

  // FIXME: the following should produce an error, because the isolated
  // conformance of C: P can cross isolation boundaries via the Sendable Self's
  // associated type.
  HoldsC.acceptSendableAliased(C.self)
}


struct MyHashable: @MainActor Hashable {
  var counter = 0
}

@concurrent func testMyHashableSet() async {
  let _: Set<MyHashable> = [] // expected-warning{{main actor-isolated conformance of 'MyHashable' to 'Hashable' cannot be used in nonisolated context}}
}
