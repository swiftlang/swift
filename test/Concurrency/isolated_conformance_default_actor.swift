// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -enable-experimental-feature IsolatedConformances -enable-experimental-feature UnspecifiedMeansMainActorIsolated %s

// REQUIRES: swift_feature_IsolatedConformances
// REQUIRES: swift_feature_UnspecifiedMeansMainActorIsolated
// REQUIRES: concurrency

nonisolated
protocol P {
  func f() // expected-note{{mark the protocol requirement 'f()' 'async' to allow actor-isolated conformances}}
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

// expected-note@+2{{add '@preconcurrency' to the 'P' conformance to defer isolation checking to run time}}
// expected-note@+1{{add '@MainActor' to the 'P' conformance to restrict it to main actor-isolated code}}
nonisolated class CNonIsolated: P {
  @MainActor func f() { } // expected-error{{main actor-isolated instance method 'f()' cannot be used to satisfy nonisolated requirement from protocol 'P'}}
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
