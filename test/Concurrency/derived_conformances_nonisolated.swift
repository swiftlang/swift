// RUN: %target-swift-frontend  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify
// RUN: %target-swift-frontend  -disable-availability-checking -strict-concurrency=complete -parse-as-library %s -emit-sil -o /dev/null -verify -strict-concurrency=complete -enable-upcoming-feature RegionBasedIsolation

// REQUIRES: concurrency
// REQUIRES: asserts

@MainActor
struct X1: Equatable, Hashable, Codable {
  let x: Int
  let y: String
}

// expected-error@+5 3{{main actor-isolated property 'y' can not be referenced from a non-isolated context}}
// expected-note@+4{{in static method '==' for derived conformance to 'Equatable'}}
// expected-error@+3{{main actor-isolated property 'y' can not be referenced from a non-isolated context}}
// expected-note@+2{{in static method '==' for derived conformance to 'Equatable'}}
@MainActor
struct X2: Equatable, Hashable, Codable {
  let x: Int
  var y: String // expected-note 4 {{property declared here}}
}

@MainActor
enum X3: Hashable, Comparable, Codable {
  case a
  case b(Int)
}

// expected-warning@+5{{main actor-isolated property 'y' can not be referenced from a non-isolated context}}
// expected-note@+4{{in static method '==' for derived conformance to 'Equatable'}}
// expected-warning@+3{{main actor-isolated property 'y' can not be referenced from a non-isolated context}}
// expected-note@+2{{in static method '==' for derived conformance to 'Equatable'}}
@preconcurrency @MainActor
struct X4: Equatable {
  let x: Int
  var y: String // expected-note 2 {{property declared here}}
}
