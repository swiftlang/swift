// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -target %target-swift-5.7-abi-triple -I %t 2>&1 %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

protocol Device: DistributedActor {
  distributed func fetch() async -> String?

  distributed func fetch2() async -> String?
  distributed func fetch5() async -> String?
}

distributed actor Impl: Device {
  typealias ActorSystem = FakeActorSystem

  init(actorSystem: ActorSystem) {
    self.actorSystem = actorSystem
  }

  // fetch() uses default impl

  distributed func fetch2() async -> String? {
    ""
  }

  distributed func fetch5() async -> String? {
    ""
  }
}

// MARK: - Device default implementation

extension Device {
  distributed func fetch() async -> String? { "" }

  distributed func fetch2() async -> String? { "" }
  distributed func fetch5() async -> String? { "" }
}
