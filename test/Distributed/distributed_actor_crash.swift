// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -typecheck -verify -disable-availability-checking -I %t 2>&1 %s
// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.5, *)
typealias DefaultDistributedActorSystem = FakeActorSystem

import Distributed

protocol DistributedWorker: DistributedActor {
  associatedtype WorkItem: Sendable & Codable
  associatedtype WorkResult: Sendable & Codable

  distributed func submit(work: WorkItem) async throws -> WorkResult
}

distributed actor TheWorker: DistributedWorker {
  typealias ActorSystem = FakeActorSystem
  typealias WorkItem = String
  typealias WorkResult = String

  distributed func submit(work: WorkItem) async throws -> WorkResult {
    work
  }
}

distributed actor WorkerPool<Worker: DistributedWorker> {
  typealias ActorSystem = FakeActorSystem
  typealias WorkItem = Worker.WorkItem
  typealias WorkResult = Worker.WorkResult

  func submit(work: WorkItem) async throws -> WorkResult {
    let worker = try await self.selectWorker()
    return try await worker.submit(work: work)
//    return try await TheWorker(actorSystem: actorSystem).submit(work: "X")
  }

  func selectWorker() async throws -> Worker {
    fatalError()
  }
}
