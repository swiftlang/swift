// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-cpu-apple-macos14 -enable-experimental-feature Embedded -enable-experimental-feature SendCalls -parse-as-library -module-name main -plugin-path %swift-plugin-dir %s %S/Inputs/EmbeddedFakeActorSystem.swift -c -o %t/a.o
// RUN: %target-embedded-link %t/a.o %target-embedded-posix-shim -o %t/a.out -L%swift_obj_root/lib/swift/embedded/%module-target-triple %target-clang-resource-dir-opt -lswift_Concurrency -lswiftDistributed %target-swift-default-executor-opt %target-embedded-concurrency-threading-shim -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=wasip1
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_SendCalls

// In Embedded Swift the distributed thunk of an 'async' '@oneway' distributed
// func is synchronous too, so 'try send' works from synchronous code: a local
// call runs on the actor in a new task, a remote call goes through the
// synchronous 'remoteCallOneway' of the '_DistributedOnewayActorSystem'

import _Concurrency
import Distributed

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

distributed actor Stepper {
  typealias ActorSystem = EmbeddedFakeRoundtripActorSystem

  var seen: [Int] = []

  @oneway distributed func step(_ n: Int) async {
    await Task.yield()
    seen.append(n)
    print("[swift] Stepper.step(\(n))")
  }

  distributed func count() -> Int { seen.count }
}

func sendSteps(_ s: Stepper, _ from: Int) throws {
  try send s.step(from)
  try send s.step(from + 1)
}

@main struct Main {
  static func main() async {
    do {
      print("[swift] test_local")
      let system = EmbeddedFakeRoundtripActorSystem()
      let local = Stepper(actorSystem: system)
      try sendSteps(local, 1)
      try await waitUntil { try await local.count() >= 2 }

      print("[swift] test_remote")
      let remote = try Stepper.resolve(id: local.id, using: system)
      try sendSteps(remote, 11)
      try await waitUntil { try await local.count() >= 4 }
    } catch {
      print("[swift] threw")
    }
    print("[swift] done")
  }
}

// CHECK-LABEL: [swift] test_local
// CHECK-NOT: remoteCallOneway
// CHECK-DAG: [swift] Stepper.step(1)
// CHECK-DAG: [swift] Stepper.step(2)
// CHECK-LABEL: [swift] test_remote
// CHECK-DAG: [swift] remoteCallOneway reached, oneway: true, onewayTarget: true
// CHECK-DAG: [swift] remoteCallOneway reached, oneway: true, onewayTarget: true
// CHECK-DAG: [swift] Stepper.step(11)
// CHECK-DAG: [swift] Stepper.step(12)
// CHECK-NOT: [swift] threw
// CHECK: [swift] done
