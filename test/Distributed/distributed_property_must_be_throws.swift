// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -verify-ignore-unknown -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

let globalActorSystem = LocalTestingDistributedActorSystem()

distributed actor MyDistributedActor {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  distributed var distributedProperty: Set<Int> { [] }
  distributed var accessMe: Set<Int> { [] }
  // expected-note@-1{{access to distributed property 'accessMe' from outside the distributed actor 'MyDistributedActor' must be asynchronous}}
}

func test(da: MyDistributedActor) async throws {
  _ = await da.distributedProperty // expected-error{{property access can throw but is not marked with 'try'}}
  // expected-note@-1{{did you mean to use 'try'?}}
  // expected-note@-2{{did you mean to handle error as optional value?}}
  // expected-note@-3{{did you mean to disable error propagation?}}

  _ = try da.distributedProperty // expected-error{{actor-isolated distributed property 'distributedProperty' cannot be accessed from outside of the actor}} {{11-11=await }}

  _ = try await da.distributedProperty // ok, implicitly async + throws
}

func testSyncFunc(da: MyDistributedActor) throws {
  _ = da.accessMe // expected-error{{actor-isolated distributed property 'accessMe' can not be referenced from a nonisolated context}}
  // expected-error@-1{{property access can throw but is not marked with 'try'}}
  // expected-note@-2{{did you mean to use 'try'?}}
  // expected-note@-3{{did you mean to handle error as optional value?}}
  // expected-note@-4{{did you mean to disable error propagation?}}
}
