// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature OnewayNowait -Xfrontend -disable-experimental-parser-round-trip -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayNowait

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

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

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  // Opted into oneway semantics: the target carries isOnewayRemoteCall == true
  var onewayDelivered = 0

  distributed func thanksOneway() oneway { onewayDelivered += 1 }

  distributed func pingOneway() async oneway { onewayDelivered += 1 }

  distributed func delivered() -> Int { onewayDelivered }

  // A plain distributed void method: isOnewayRemoteCall stays false
  distributed func plainVoid() {}
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()

    // A local actor whose system always resolves references as remote, so calls
    // drive the remote branch of the synthesized thunk. That branch invokes
    // remoteCallVoid; for 'oneway' targets the target's isOnewayRemoteCall flag
    // is set, which the system observes and reports
    let local = Greeter(actorSystem: system)
    let ref = try Greeter.resolve(id: local.id, using: system)

    // ==== Oneway methods carry the flag, both the sync and the async one.
    // 'nowait' does not wait for delivery and does not order the two calls
    // relative to each other, so wait until both bodies ran
    nowait ref.thanksOneway()
    nowait ref.pingOneway()
    try await waitUntil { try await local.delivered() >= 2 }
    // CHECK: >> remoteCallVoid: is oneway call
    // CHECK: >> remoteCallVoid: is oneway call

    // ==== Plain distributed void method does NOT report the flag
    try await ref.plainVoid()
    // CHECK-NOT: is oneway call

    print("done")
    // CHECK: done
  }
}
