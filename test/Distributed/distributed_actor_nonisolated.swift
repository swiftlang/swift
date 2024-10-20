// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA {

  let local: Int = 42
  // expected-note@-1{{access to property 'local' is only permitted within distributed actor 'DA'}}
  // expected-note@-2{{access to property 'local' is only permitted within distributed actor 'DA'}}

  nonisolated let nope: Int = 13
  // expected-error@-1{{'nonisolated' can not be applied to distributed actor stored properties}}

  nonisolated var computedNonisolated: Int {
    // nonisolated computed properties are outside of the actor and as such cannot access local
    _ = self.local // expected-error{{distributed actor-isolated property 'local' can not be accessed from a nonisolated context}}

    _ = self.id // ok, special handled and always available
    _ = self.actorSystem // ok, special handled and always available
  }

  distributed func dist() {}

  nonisolated func access() async throws {
    _ = self.id // ok
    _ = self.actorSystem // ok
    
    // self is a distributed actor self is NOT isolated
    _ = self.local // expected-error{{distributed actor-isolated property 'local' can not be accessed from a nonisolated context}}
    _ = try await self.dist() // ok, was made implicitly throwing and async
    _ = self.computedNonisolated // it's okay, only the body of computedNonisolated is wrong
  }

  nonisolated distributed func nonisolatedDistributed() async {
    // expected-error@-1{{cannot declare method 'nonisolatedDistributed()' as both 'nonisolated' and 'distributed'}}{{3-15=}}
    fatalError()
  }

  distributed nonisolated func distributedNonisolated() async {
    // expected-error@-1{{cannot declare method 'distributedNonisolated()' as both 'nonisolated' and 'distributed'}}{{15-27=}}
    fatalError()
  }

}

func invalidIsolatedCall<DA: DistributedActor> (
  to actor: DA,
  queue: AsyncStream<@Sendable (isolated DA) async -> Void>
) {
  Task {
    // expected-note@+1 {{let declared here}}
    for await closure in queue {
      // expected-warning@+1 {{distributed actor-isolated let 'closure' can not be accessed from a nonisolated context; this is an error in the Swift 6 language mode}}
      await closure(actor)
    }
  }
}
