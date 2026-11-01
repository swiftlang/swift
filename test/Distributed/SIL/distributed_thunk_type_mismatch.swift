// RUN: %target-swift-emit-silgen -module-name main %s -verify

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed

struct Outer<T> {
  struct Inner: Codable {}
}

distributed actor A {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed func f(_: Outer<Int>.Inner) {} // expected-note {{function declared here}}
}

extension A {
  @_silgen_name("$s4main1AC1fyyAA5OuterV5InnerVySi_GYaKFTE")
  nonisolated func g(_: sending Outer<Bool>.Inner) async throws
}

func use(_ a: A) async throws {
  try await a.g(Outer<Bool>.Inner()) // expected-error {{function type mismatch}}
}
