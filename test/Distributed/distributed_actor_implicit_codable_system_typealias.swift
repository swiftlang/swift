// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

distributed actor DA {
  typealias ActorSystem = FakeActorSystem

  init(actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
  }
}

func take<A: Codable>(actor: A) {}
func takeE<A: Encodable>(actor: A) {}
func takeD<A: Decodable>(actor: A) {}

func test(actorSystem: FakeActorSystem) {
  take(actor: DA(actorSystem: actorSystem)) // ok
  takeE(actor: DA(actorSystem: actorSystem)) // ok
  takeD(actor: DA(actorSystem: actorSystem)) // ok
}
