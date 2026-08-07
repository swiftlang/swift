// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature DistributedRemoteCallSemantics -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_DistributedRemoteCallSemantics

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  @remoteCall(blocking)
  distributed func helloBlocking() -> String { "hello-blocking" } // remoteCall

  @remoteCall(blocking)
  distributed func pingBlocking() {} // remoteCallVoid

  distributed func helloAsync() -> String { "hello-async" }

  @remoteCall(blocking)
  distributed var statusBlocking: String { "status-blocking" }

  distributed var statusAsync: String { "status-async" }
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()

    let local = Greeter(actorSystem: system)
    let ref = try Greeter.resolve(id: local.id, using: system)

    // ==== Blocking method carries the flag
    let value = try await ref.helloBlocking()
    // CHECK: >> remoteCall: is synchronous blocking call
    print("helloBlocking() returned: \(value)")
    // CHECK: helloBlocking() returned: hello-blocking

    // ==== Blocking void method carries the flag
    try await ref.pingBlocking()
    // CHECK: >> remoteCallVoid: is synchronous blocking call

    // ==== Blocking computed property carries the flag
    let status = try await ref.statusBlocking
    // CHECK: >> remoteCall: is synchronous blocking call
    print("statusBlocking returned: \(status)")
    // CHECK: statusBlocking returned: status-blocking

    // ==== Non-blocking method does NOT report the flag
    let asyncValue = try await ref.helloAsync()
    // CHECK-NOT: is synchronous blocking call
    print("helloAsync() returned: \(asyncValue)")
    // CHECK: helloAsync() returned: hello-async

    // ==== Non-blocking computed property does NOT report the flag
    let asyncStatus = try await ref.statusAsync
    print("statusAsync returned: \(asyncStatus)")
    // CHECK: statusAsync returned: status-async

    print("done")
    // CHECK: done
  }
}
