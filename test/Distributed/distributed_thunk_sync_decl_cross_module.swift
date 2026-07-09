// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule -O -parse-as-library -target %target-swift-5.7-abi-triple -I %t %s -DLIB
// RUN: %target-swift-frontend -emit-sil -module-name Client -O -parse-as-library -target %target-swift-5.7-abi-triple -I %t %s -DCLIENT -o /dev/null

// REQUIRES: concurrency
// REQUIRES: distributed
// UNSUPPORTED: back_deploy_concurrency

// Reproduces a SILFunction type mismatch uncovered by DistributedCluster.
// This happens only for a synchronous distributed var decl, across modules.

import Distributed
import FakeDistributedActorSystems

#if LIB

public protocol DistWorker: DistributedActor where ActorSystem == FakeActorSystem {
  associatedtype Item: Codable & Sendable
  distributed func handle(item: Item) async throws
}

public distributed actor WorkerPool<W: DistWorker>: DistributedActor {
  public typealias ActorSystem = FakeActorSystem
  var workers: [W] = []

  public distributed var size: Int {
    self.workers.count
  }

  public distributed func count() async throws -> Int {
    self.workers.count
  }
}

#endif

#if CLIENT
import Lib

public struct Job: Codable, Sendable {
  public let id: Int
  public init(id: Int) { self.id = id }
}

public distributed actor ExampleWorker: DistWorker {
  public typealias ActorSystem = FakeActorSystem
  public typealias Item = Job

  public distributed func handle(item: Job) async throws {
    _ = item.id
  }
}

public func use(_ system: FakeActorSystem) async throws {
  let pool = WorkerPool<ExampleWorker>(actorSystem: system)
  // Reading 'pool.size' across an isolation boundary forces the
  // distributed thunk to be deserialized.
  _ = try await pool.size
  _ = try await pool.count()
}
#endif
