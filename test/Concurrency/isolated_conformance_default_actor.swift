// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.1-abi-triple -swift-version 6 -default-isolation MainActor %s

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

// Synthesized conformances
struct EquatableStruct: Equatable {
  var state: Int = 0
}

struct HashableStruct: Hashable {
  var state: Int = 0
}

enum RawRepresentableEnum: Int {
case one
case two
}

class CodableClass: Codable {
  var state: Int = 0
}

class OtherClass {
  var otherState: any Encodable.Type = CodableClass.self
}

struct Landmark: Codable {
  var name: String
  var foundingYear: Int
}

func acceptSendablePMeta<T: Sendable & P>(_: T.Type) { }
func acceptSendableQMeta<T: Sendable & Q>(_: T.Type) { }

nonisolated func testConformancesFromNonisolated() {
  let _: any P = CExplicitMainActor() // okay
  let _: any P = CImplicitMainActor() // okay

  let _: any P = CNonIsolated()
  let _: any P = CImplicitMainActorNonisolatedConformance()

  // Okay, these are nonisolated conformances.
  let _: any Q = CExplicitMainActor()
  let _: any Q = CImplicitMainActor()

  // Error, these are main-actor-isolated conformances
  let _: any Equatable.Type = EquatableStruct.self // expected-error{{main actor-isolated conformance of 'EquatableStruct' to 'Equatable' cannot be used in nonisolated context}}
  let _: any Hashable.Type = HashableStruct.self // expected-error{{main actor-isolated conformance of 'HashableStruct' to 'Hashable' cannot be used in nonisolated context}}
  let _: any RawRepresentable.Type = RawRepresentableEnum.self
  let _: any Encodable.Type = CodableClass.self // expected-error{{main actor-isolated conformance of 'CodableClass' to 'Encodable' cannot be used in nonisolated context}}
}
