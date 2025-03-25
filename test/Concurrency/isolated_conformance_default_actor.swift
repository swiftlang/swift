// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -enable-experimental-feature IsolatedConformances -default-isolation MainActor %s

// REQUIRES: swift_feature_IsolatedConformances
// REQUIRES: concurrency

nonisolated
protocol P {
  func f()
}

@MainActor
protocol Q {
  func g()
}

class CImplicitMainActorNonisolatedConformance: nonisolated P {
  func f() { } // error: explicitly nonisolated conformance
}


@MainActor
class CExplicitMainActor: P {
  func f() { } // okay! conformance above is isolated
}

class CImplicitMainActor: P {
  func f() { } // okay! conformance above is isolated
}

// If the protocol itself is isolated, don't do anything.
extension CExplicitMainActor: Q {
  func g() { }
}

extension CImplicitMainActor: Q {
  func g() { }
}

// expected-error@+3{{conformance of 'CNonIsolated' to protocol 'P' crosses into main actor-isolated code and can cause data races}}
// expected-note@+2{{turn data races into runtime errors with '@preconcurrency'}}
// expected-note@+1{{isolate this conformance to the main actor with '@MainActor'}}{{33-33=@MainActor }}
nonisolated class CNonIsolated: P {
  @MainActor func f() { } // expected-note{{main actor-isolated instance method 'f()' cannot satisfy nonisolated requirement}}
}

func acceptSendablePMeta<T: Sendable & P>(_: T.Type) { }
func acceptSendableQMeta<T: Sendable & Q>(_: T.Type) { }

nonisolated func testConformancesFromNonisolated() {
  let _: any P = CExplicitMainActor() // expected-error{{main actor-isolated conformance of 'CExplicitMainActor' to 'P' cannot be used in nonisolated context}}
  let _: any P = CImplicitMainActor() // expected-error{{main actor-isolated conformance of 'CImplicitMainActor' to 'P' cannot be used in nonisolated context}}

  let _: any P = CNonIsolated()
  let _: any P = CImplicitMainActorNonisolatedConformance()

  // Okay, these are nonisolated conformances.
  let _: any Q = CExplicitMainActor()
  let _: any Q = CImplicitMainActor()
}
