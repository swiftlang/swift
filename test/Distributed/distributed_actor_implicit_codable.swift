// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor DA {
}

func take<A: Codable>(actor: A) {}

func test(actorSystem: FakeActorSystem) {
  take(actor: DA(actorSystem: actorSystem)) // ok
}

func takeCodable<A: Codable>(actor: A) {}

func test_DA(actorSystem: FakeActorSystem) {
  takeCodable(actor: DA(actorSystem: actorSystem)) // ok
}

// ==== Generic actors

distributed actor DAG<ActorSystem>
  where ActorSystem: DistributedActorSystem<any Codable> {
}
func test_DAG(actorSystem: FakeActorSystem) {
  takeCodable(actor: DAG<FakeActorSystem>(actorSystem: actorSystem)) // ok
}

distributed actor DAG_ID<ActorSystem>
  where ActorSystem: DistributedActorSystem<any Codable>,
        ID: Codable { // expected-error{{cannot find type 'ID' in scope}}
}
func test_DAG_ID(actorSystem: FakeActorSystem) {
  takeCodable(actor: DAG_ID<FakeActorSystem>(actorSystem: actorSystem)) // ok
}

distributed actor DAG_ActorSystem_ActorID<ActorSystem>
  where ActorSystem: DistributedActorSystem<any Codable>,
        ActorSystem.ActorID: Codable {
}
func test_DAG_ActorSystem_ActorID(actorSystem: FakeActorSystem) {
  takeCodable(actor: DAG_ActorSystem_ActorID<FakeActorSystem>(actorSystem: actorSystem)) // ok
}

// ==== Not codable cases

protocol SerializableButNotCodable {}

distributed actor DAG_ActorSystem_ActorID_Custom<ActorSystem>
  where ActorSystem: DistributedActorSystem<any SerializableButNotCodable> {
  // expected-note@-2{{requirement specified as 'ActorSystem.SerializationRequirement' == 'any SerializableButNotCodable'}}
}

func test_DAG_ActorSystem_ActorID_Custom(actorSystem: FakeActorSystem) {
  takeCodable(actor: DAG_ActorSystem_ActorID_Custom<FakeActorSystem>(actorSystem: actorSystem))
  // expected-error@-1{{'DAG_ActorSystem_ActorID_Custom' requires the types 'any FakeActorSystem.SerializationRequirement' (aka 'any Decodable & Encodable') and 'any SerializableButNotCodable' be equivalent}}
}
