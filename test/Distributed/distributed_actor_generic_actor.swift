// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -enable-experimental-distributed -target %target-swift-5.7-abi-triple -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

distributed actor Worker<Work: Sendable & Codable> {
  typealias ActorSystem = FakeActorSystem

  distributed func echo(item: Work) -> Work {
    item
  }

  distributed func echo(items: [Work]) -> [Work] {
    items
  }

  distributed func other<Other: Codable & Sendable>(other: Other) -> Other {
    other
  }

  distributed func others<Other: Codable & Sendable>(other: Other) -> [Other] {
    [other]
  }
}
