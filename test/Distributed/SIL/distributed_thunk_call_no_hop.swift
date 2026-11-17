// RUN: %target-swift-emit-silgen %s -module-name main -target %target-swift-5.7-abi-triple | %FileCheck %s
// RUN: %target-swift-emit-silgen %s -module-name main -target %target-swift-5.7-abi-triple -enable-sil-opaque-values | %FileCheck %s
// REQUIRES: concurrency
// REQUIRES: distributed

// A cross-actor call to a synchronous `distributed` func or var is dispatched
// through the distributed thunk, which is async and nonisolated. The caller must
// not hop to the actor first: it may be a remote reference, whose executor must
// never be hopped to. The thunk hops to the actor itself when it is local.

import Distributed

protocol Worker: DistributedActor where ActorSystem == LocalTestingDistributedActorSystem {
  associatedtype Item: Codable & Sendable
  distributed func work(_ item: Item) -> String
}

distributed actor TheWorker: Worker {
  typealias ActorSystem = LocalTestingDistributedActorSystem
  typealias Item = String

  distributed func work(_ item: String) -> String { item }
  distributed var name: String { "worker" }
}

// CHECK-LABEL: sil hidden {{.*}}@$s4main6directySSAA9TheWorkerCYaKF :
// CHECK:       bb0([[W:%.*]] : @guaranteed $TheWorker):
// CHECK-NOT:     hop_to_executor [[W]]
// CHECK:         [[THUNK:%.*]] = function_ref @$s4main9TheWorkerC4workyS2SYaKFTE :
// CHECK-NOT:     hop_to_executor [[W]]
// CHECK:         try_apply [[THUNK]]({{%.*}}, [[W]])
// CHECK:       } // end sil function '$s4main6directySSAA9TheWorkerCYaKF'
func direct(_ w: TheWorker) async throws -> String {
  try await w.work("direct")
}

// CHECK-LABEL: sil hidden {{.*}}@$s4main7genericySSxYaKAA6WorkerRzSS4ItemRtzlF :
// CHECK:       bb0([[W:%.*]] : @guaranteed $W):
// CHECK-NOT:     hop_to_executor [[W]]
// CHECK:         [[THUNK:%.*]] = witness_method $W, #Worker.work!distributed_thunk :
// CHECK-NOT:     hop_to_executor [[W]]
// CHECK:         try_apply [[THUNK]]<W>({{%.*}}, [[W]])
// CHECK:       } // end sil function '$s4main7genericySSxYaKAA6WorkerRzSS4ItemRtzlF'
func generic<W: Worker>(_ w: W) async throws -> String where W.Item == String {
  try await w.work("generic")
}

// CHECK-LABEL: sil hidden {{.*}}@$s4main8propertyySSAA9TheWorkerCYaKF :
// CHECK:       bb0([[W:%.*]] : @guaranteed $TheWorker):
// CHECK-NOT:     hop_to_executor [[W]]
// CHECK:         [[THUNK:%.*]] = function_ref @$s4main9TheWorkerC4nameSSyYaKFTE :
// CHECK-NOT:     hop_to_executor [[W]]
// CHECK:         try_apply [[THUNK]]([[W]])
// CHECK:       } // end sil function '$s4main8propertyySSAA9TheWorkerCYaKF'
func property(_ w: TheWorker) async throws -> String {
  try await w.name
}
