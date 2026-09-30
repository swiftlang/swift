// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature OnewayNowait -disable-experimental-parser-round-trip -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// 'nowait' on a LOCAL distributed-actor target must dispatch to the actor's
// own method (the local branch of the distributed thunk), NOT 'remoteCall'.
// SILGen checks 'isRemote' at runtime and, for a local instance, enqueues the
// fire-and-forget task on the actor's own serial executor. This test verifies
// that every 'nowait local.step(i)' actually runs the local method body.
//
// NOTE on ordering: strict submission-order FIFO for distributed targets is NOT
// asserted here. The synthesized distributed thunk is currently '@concurrent'
// and hops to the generic executor on entry (before its own 'isRemote' check),
// so a burst of 'nowait' calls round-trips through the global pool and can
// reorder. End-to-end FIFO for distributed targets becomes exact once the
// distributed thunk is 'nonisolated(nonsending)' (tracked separately) and no
// longer performs that generic-executor entry hop. Non-distributed actors do
// get exact FIFO today; see Concurrency/Runtime/nowait_fifo.swift

import Distributed
import FakeDistributedActorSystems

// Polls until 'done' returns true, and fails instead of hanging forever if the
// fire-and-forget calls never run
func waitUntil(_ done: () async throws -> Bool) async rethrows {
  for _ in 0 ..< 1_000_000 {
    if try await done() {
      return
    }
    await Task.yield()
  }
  fatalError("timed out waiting for the 'nowait' calls to run")
}

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Stepper {
  var seen: Set<Int> = []

  distributed func step(_ n: Int) oneway {
    seen.insert(n)
  }

  distributed func count() -> Int { seen.count }
  distributed func sortedSeen() -> [Int] { seen.sorted() }
}

@main struct Main {
  static func main() async throws {
    let system = FakeActorSystem()
    let local = Stepper(actorSystem: system)

    for i in 0 ..< 8 {
      nowait local.step(i)
    }

    // Fire-and-forget: wait until all local sends have run, then check that
    // each one dispatched to the local method (completeness, order-independent)
    try await waitUntil { try await local.count() >= 8 }

    let all = try await local.sortedSeen()
    print("seen: \(all)")
    // CHECK: seen: [0, 1, 2, 3, 4, 5, 6, 7]
  }
}
