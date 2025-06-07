// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -enable-upcoming-feature GlobalActorIsolatedTypesUsability
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation -enable-upcoming-feature GlobalActorIsolatedTypesUsability

// REQUIRES: concurrency
// REQUIRES: swift_feature_GlobalActorIsolatedTypesUsability
// REQUIRES: swift_feature_RegionBasedIsolation

@MainActor
struct X1: Equatable, Hashable, Codable {
  let x: Int
  let y: String
}

// okay
@MainActor
struct X2: Equatable, Hashable, Codable {
  let x: Int
  nonisolated var y: String // okay
}

class NonSendable {
  let x: Int

  init(x: Int) {
    self.x = x
  }
}

extension NonSendable: Equatable {
  static func == (lhs: NonSendable, rhs: NonSendable) -> Bool {
    return lhs.x == rhs.x
  }
}

// expected-warning@+3 2{{main actor-isolated property 'x' can not be referenced from a nonisolated context}}
// expected-note@+2 2{{in static method '==' for derived conformance to 'Equatable'}}
@MainActor
struct X2NonSendable: Equatable {
  let x: NonSendable // expected-note 2 {{property declared here}}
}

@MainActor
enum X3: Hashable, Comparable, Codable {
  case a
  case b(Int)
}

// okay
@preconcurrency @MainActor
struct X4: Equatable {
  let x: Int
  var y: String
}
