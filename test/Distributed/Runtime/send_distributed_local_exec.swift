// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature SendCalls -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

// 'send' on a LOCAL distributed-actor target must dispatch to the actor's
// own method (the local branch of the distributed thunk), NOT 'remoteCall'.
// The synchronous distributed thunk of the '@oneway' function checks
// 'isRemote' at runtime and, for a local instance, enqueues the
// fire-and-forget task on the actor's own serial executor. This test verifies
// that every 'try send local.step(i)' actually runs the local method body, in
// the order the calls were made: the thunk creates the task on the caller's
// executor and the task starts on the actor's executor, so there is no hop
// in between which could reorder them. See also
// Concurrency/Runtime/send_fifo.swift

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
  fatalError("timed out waiting for the 'send' calls to run")
}

typealias DefaultDistributedActorSystem = FakeActorSystem

distributed actor Stepper {
  var seen: [Int] = []

  @oneway distributed func step(_ n: Int) {
    seen.append(n)
  }

  distributed func count() -> Int { seen.count }
  distributed func allSeen() -> [Int] { seen }
}

@main struct Main {
  static func main() async throws {
    let system = FakeActorSystem()
    let local = Stepper(actorSystem: system)

    for i in 0 ..< 8 {
      try send local.step(i)
    }

    // Fire-and-forget: wait until all local sends have run, then check that
    // each one dispatched to the local method, in order
    try await waitUntil { try await local.count() >= 8 }

    let all = try await local.allSeen()
    print("seen: \(all)")
    // CHECK: seen: [0, 1, 2, 3, 4, 5, 6, 7]
  }
}
