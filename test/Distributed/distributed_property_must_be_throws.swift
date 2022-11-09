// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

let globalActorSystem = LocalTestingDistributedActorSystem()

distributed actor MyDistributedActor {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed var distributedProperty: Set<Int> { [] }
}

actor MyActor {
  init(distributedActor: MyDistributedActor) {
    self.distributedActor = distributedActor
  }

  let distributedActor: MyDistributedActor

  static func makeActor(actorSystem: LocalTestingDistributedActorSystem) async throws -> MyActor {
    let distributedActor = try MyDistributedActor.resolve(id: LocalTestingActorID(id: "42"), using: actorSystem)
    return MyActor(distributedActor: distributedActor)
  }
}

class Caller {
  public var value: Set<Int> {
    get async {
      var value: Set<Int>
      do {
        let actor = try await makeActor(actorSystem: globalActorSystem)
        let da = actor.distributedActor

        _ = await da.distributedProperty // expected-error{{property access can throw but is not marked with 'try'}}
        // expected-note@-1{{did you mean to use 'try'?}}
        // expected-note@-2{{did you mean to handle error as optional value?}}
        // expected-note@-3{{did you mean to disable error propagation?}}

        _ = try da.distributedProperty // expected-error{{expression is 'async' but is not marked with 'await'}}
        // expected-note@-1{{property access is 'async'}}

        value = try await da.distributedProperty // ok, implicitly async + throws
      } catch {
        fatalError("\(error)")
      }
      return value
    }
  }

  func makeActor(actorSystem: LocalTestingDistributedActorSystem) async throws -> MyActor {
    try await MyActor.makeActor(actorSystem: actorSystem)
  }
}
