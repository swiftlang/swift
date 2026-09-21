// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/Inputs/FakeDistributedActorSystems.swift
// RUN: %target-swift-frontend -Xllvm -sil-print-types -emit-silgen %s -module-name test -swift-version 5 -target %target-swift-5.7-abi-triple -I %t | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: distributed

import Distributed
import FakeDistributedActorSystems

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

// The synchronous 'distributed var size' thunk must take a nonisolated '@guaranteed self', not '@sil_isolated @guaranteed self'.
// CHECK-LABEL: sil [thunk] [distributed] {{.*}}@$s4test10WorkerPoolC4sizeSiyYaKFTE :
// CHECK-SAME:  $@convention(method) @async <W where W : DistWorker> (@guaranteed WorkerPool<W>) -> (Int, @error any Error)

// The 'distributed func count()' thunk must also be nonisolated
// '@guaranteed self'.
// CHECK-LABEL: sil [thunk] [distributed] {{.*}}@$s4test10WorkerPoolC5countSiyYaKFTE :
// CHECK-SAME:  $@convention(method) @async <W where W : DistWorker> (@guaranteed WorkerPool<W>) -> (Int, @error any Error)
