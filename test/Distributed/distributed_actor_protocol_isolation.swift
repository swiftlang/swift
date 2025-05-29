// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol Greeting: DistributedActor {
  distributed func greeting() -> String
}

extension Greeting {
  func greetLocal(name: String) { // expected-note{{distributed actor-isolated instance method 'greetLocal(name:)' declared here}}
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

extension Greeting where Self.SerializationRequirement == Codable {
  nonisolated func greetAliceALot() async throws {
    try await self.greetDistributed(name: "Alice") // okay, via Codable
    let rawGreeting = try await greeting() // okay, via Self's serialization requirement
    _ = rawGreeting

    greetLocal(name: "Alice") // expected-error{{only 'distributed' instance methods can be called on a potentially remote distributed actor}}
  }
}
