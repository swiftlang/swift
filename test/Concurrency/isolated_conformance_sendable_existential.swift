// RUN: %target-swift-frontend -swift-version 6 -emit-sil -verify %s -enable-upcoming-feature IsolatedConformances

// REQUIRES: concurrency
// REQUIRES: asserts
// REQUIRES: swift_feature_IsolatedConformances

@globalActor
actor AnotherActor: GlobalActor {
  static let shared = AnotherActor()
}

protocol P {
  nonisolated(nonsending) func foo(_ ns: NS) async
}

protocol Q {
  func bar()
}

protocol SendableThing: Sendable {
  func baz()
}

class NS {
  var k: Int = 0
}

@MainActor
class C: @MainActor P {
  @MainActor
  func foo(_ ns: NS) async {
  }
}

@MainActor
class CQ: @MainActor P, @MainActor Q {
  @MainActor
  func foo(_ ns: NS) async {
  }
  @MainActor
  func bar() {
  }
}

@MainActor
class CSendable: @MainActor P, Sendable {
  @MainActor
  func foo(_ ns: NS) async {
  }
}

@MainActor
final class CSendableThing: @MainActor P, SendableThing { // expected-error{{conformance of 'CSendableThing' to protocol 'SendableThing' crosses into main actor-isolated code and can cause data races}}
  // expected-note@-1{{isolate this conformance to the main actor with '@MainActor'}}
  // expected-note@-2{{turn data races into runtime errors with '@preconcurrency'}}
  @MainActor
  func foo(_ ns: NS) async {
  }
  func baz() { // expected-note{{main actor-isolated instance method 'baz()' cannot satisfy nonisolated requirement}}
  // expected-note@-1{{mark instance method 'baz()' 'nonisolated'}}
  }
}

@AnotherActor
func test<T: P & Sendable>(_ x: T) async { // expected-note{{'test' declared here}}
  await x.foo(NS())
}

// ==== Isolated conformance escapes via Sendable existential

@MainActor
func callIt() async {
  await test(C()) // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot satisfy conformance requirement for a 'Sendable' type parameter}}

  let s: any P & Sendable = C() // expected-error{{main actor-isolated conformance of 'C' to 'P' cannot be used to satisfy a 'Sendable' existential type}}
  await test(s)
}

// === Multiple protocols: any P & Q & Sendable

@MainActor
func callMultiProto() async {
  let s: any P & Q & Sendable = CQ() // expected-error{{main actor-isolated conformance of 'CQ' to 'P' cannot be used to satisfy a 'Sendable' existential type}}
  _ = s
}

@MainActor
func callMultiProto(pq: any (P & Q)) async {
  let s: any P & Q & Sendable = pq // expected-error{{type 'any P & Q' does not conform to the 'Sendable' protocol}}
  _ = s
}

// === Class that is itself Sendable with isolated conformance

@MainActor
func callSendableClass() async {
  let s: any P & Sendable = CSendable() // expected-error{{main actor-isolated conformance of 'CSendable' to 'P' cannot be used to satisfy a 'Sendable' existential type}}
  _ = s
}

// ==== Inherited Sendable in LHS protocol

@MainActor
func callSendableThing() async {
  let s: any P & SendableThing = CSendableThing() // expected-error{{main actor-isolated conformance of 'CSendableThing' to 'P' cannot be used to satisfy a 'Sendable' existential type}}
  _ = s
}
