// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

struct SomeLogger {}
struct Logger {
  let label: String
  func info(_: String) {}
}

distributed actor Philosopher {
  let log: Logger
  // expected-note@-1{{access to property 'log' is only permitted within distributed actor 'Philosopher'}}

  var variable = 12
  var variable_fromDetach = 12
  let INITIALIZED: Int
  let outside: Int = 1

  init(system: FakeActorSystem) {
    self.log = Logger(label: "name")
    self.INITIALIZED = 1
  }

  distributed func dist() -> Int {}

  func test() {
    _ = self.id
    _ = self.actorSystem
    Task {
      _ = self.id
      _ = self.actorSystem

      self.log.info("READY!")
      _ = self.variable
      _ = self.dist()
    }

    Task.detached {
      _ = self.id
      _ = self.actorSystem

      // This is an interesting case, since we have a real local `self` and
      // yet are not isolated to the same actor in this detached task...
      // the call to it is implicitly async, however it is NOT implicitly throwing
      // because we KNOW this is a local call -- and there is no system in
      // between that will throw.
      _ = await self.dist() // notice lack of 'try' even though 'distributed func'
      _ = self.variable_fromDetach // expected-error{{actor-isolated property 'variable_fromDetach' cannot be accessed from outside of the actor}} {{11-11=await }}
      _ = await self.variable_fromDetach // okay, we know we're on the local node
    }
  }
}

func test_outside(system: FakeActorSystem) async throws {
  _ = try await Philosopher(system: system).dist()
  _ = Philosopher(system: system).log // expected-error{{distributed actor-isolated property 'log' can not be accessed from a nonisolated context}}

  _ = Philosopher(system: system).id
  _ = Philosopher(system: system).actorSystem
}

func test_outside_isolated(phil: isolated Philosopher) async throws {
  phil.log.info("works on isolated")
}
