// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend-emit-module -I %t -emit-module-path %t/distributed_actor_protocol_isolation.swiftmodule -module-name distributed_actor_protocol_isolation -disable-availability-checking %s
// X: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol Greeting: DistributedActor {
  distributed func greeting() -> String
}

extension Greeting {
  func greetLocal(name: String) {
    print("\(greeting()), \(name)!") // okay, we're on the actor
  }
}

extension Greeting where SerializationRequirement == Codable {
  // okay, uses Codable to transfer arguments.
  distributed func greetDistributed(name: String) {
    // okay, we're on the actor
    greetLocal(name: name)
  }
}

extension Greeting {
  nonisolated func greetAliceALot() async throws {
//    try await greetDistributed(name: "Alice") // okay, via Codable
//    let rawGreeting = try await greeting() // okay, via Self's serialization requirement
//    greetLocal(name: "Alice") // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  }
}
