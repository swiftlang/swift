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

@MainActor
func greetOnMainActor() {}
// expected-note@-1:6 {{calls to global function 'greetOnMainActor()' from outside of its actor context are implicitly asynchronous}}

@MainActor
protocol MainActorGreeting {
  func greet()
}

// Distributed actors shouldn't infer a global actor isolation from a protocol!
// Beyond the normal issues, that would also conflict with DistributedActor.
distributed actor MainActorGreeter: MainActorGreeting {
  @MainActor func greet() {}

  static func notAWitness() {
    // expected-note@-1:15 {{add '@MainActor' to make static method 'notAWitness()' part of global actor 'MainActor'}}
    greetOnMainActor()
    // expected-error@-1:5 {{call to main actor-isolated global function 'greetOnMainActor()' in a synchronous nonisolated context}}
  }
}
