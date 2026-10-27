// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature SendCalls -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out > %t/out.txt
// RUN: %FileCheck %s < %t/out.txt
// RUN: %FileCheck %s --check-prefix=RECIPIENT < %t/out.txt

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_SendCalls

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
  fatalError("timed out waiting for the 'send' calls to run")
}

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  // Opted into oneway semantics: the target carries isOnewayRemoteCall == true
  var onewayDelivered = 0

  @oneway distributed func thanksOneway() { onewayDelivered += 1 }

  @oneway distributed func pingOneway() async { onewayDelivered += 1 }

  distributed func delivered() -> Int { onewayDelivered }

  // A plain distributed void method: isOnewayRemoteCall stays false
  distributed func plainVoid() {}
}

@main struct Main {
  static func main() async throws {
    FakeRecipientTargetPrinting.isEnabled = true
    let system = FakeRoundtripActorSystem()

    // A local actor whose system always resolves references as remote, so calls
    // drive the remote branch of the synthesized thunk. For '@oneway' targets
    // that branch invokes the synchronous remoteCallOneway, and the target's
    // isOnewayRemoteCall flag is set, which the system reports
    let local = Greeter(actorSystem: system)
    let ref = try Greeter.resolve(id: local.id, using: system)

    // ==== Oneway methods carry the flag, both the sync and the async one.
    // 'send' does not wait for delivery, so wait until both bodies ran
    try send ref.thanksOneway()
    try send ref.pingOneway()
    try await waitUntil { try await local.delivered() >= 2 }
    // CHECK: >> remoteCallOneway: {{.*}}target:main.Greeter.thanksOneway(), {{.*}}oneway: true
    // CHECK: >> remoteCallOneway: {{.*}}target:main.Greeter.pingOneway(), {{.*}}oneway: true
    // The recipient sees the flags the sender set
    // RECIPIENT-DAG: >> executeDistributedTarget: {{.*}}thanksOneway{{.*}} oneway: true target: true
    // RECIPIENT-DAG: >> executeDistributedTarget: {{.*}}pingOneway{{.*}} oneway: true target: true

    // ==== Plain distributed void method does NOT report the flag
    try await ref.plainVoid()
    // RECIPIENT: >> executeDistributedTarget: {{.*}}plainVoid{{.*}} oneway: false target: false
    // CHECK-NOT: is oneway call
    // CHECK-NOT: remoteCallOneway

    print("done")
    // CHECK: done
  }
}
