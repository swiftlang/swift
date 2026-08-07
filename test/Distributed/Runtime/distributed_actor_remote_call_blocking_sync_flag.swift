// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature DistributedRemoteBlockingCalls -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_DistributedRemoteBlockingCalls

// rdar://184300760

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  // Opted into blocking IPC: the target carries isSynchronousBlockingCall == true.
  @remoteCall(blocking)
  distributed func helloBlocking() -> String { "hello-blocking" }

  @remoteCall(blocking)
  distributed func pingBlocking() {}

  // A plain distributed method: isSynchronousBlockingCall stays false.
  distributed func helloAsync() -> String { "hello-async" }

  // Opted-in blocking computed property.
  @remoteCall(blocking)
  distributed var statusBlocking: String { "status-blocking" }

  // A plain distributed computed property: isSynchronousBlockingCall stays false.
  distributed var statusAsync: String { "status-async" }
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()

    // A local actor whose system always resolves references as remote, so calls
    // drive the remote branch of the synthesized thunk. That branch invokes the
    // ordinary remoteCall / remoteCallVoid; for '@remoteCall(blocking)' targets
    // the target's isSynchronousBlockingCall flag is set, which the system
    // observes and reports.
    let local = Greeter(actorSystem: system)
    let ref = try Greeter.resolve(id: local.id, using: system)

    // ==== Blocking method carries the flag.
    let value = try await ref.helloBlocking()
    // CHECK: >> remoteCall: is synchronous blocking call
    print("helloBlocking() returned: \(value)")
    // CHECK: helloBlocking() returned: hello-blocking

    // ==== Blocking void method carries the flag.
    try await ref.pingBlocking()
    // CHECK: >> remoteCallVoid: is synchronous blocking call

    // ==== Blocking computed property carries the flag.
    let status = try await ref.statusBlocking
    // CHECK: >> remoteCall: is synchronous blocking call
    print("statusBlocking returned: \(status)")
    // CHECK: statusBlocking returned: status-blocking

    // ==== Non-blocking method does NOT report the flag; proving the opt-in.
    let asyncValue = try await ref.helloAsync()
    // CHECK-NOT: is synchronous blocking call
    print("helloAsync() returned: \(asyncValue)")
    // CHECK: helloAsync() returned: hello-async

    // ==== Non-blocking computed property does NOT report the flag.
    let asyncStatus = try await ref.statusAsync
    print("statusAsync returned: \(asyncStatus)")
    // CHECK: statusAsync returned: status-async

    print("done")
    // CHECK: done
  }
}
