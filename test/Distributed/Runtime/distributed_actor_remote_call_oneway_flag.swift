// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-6.2-abi-triple -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -target %target-swift-6.2-abi-triple -Xfrontend -disable-availability-checking -enable-experimental-feature OnewayMethods -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: swift_feature_OnewayMethods

// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Greeter {
  // Opted into oneway semantics: the target carries isOnewayRemoteCall == true.
  distributed func thanksOneway() oneway {}

  distributed func pingOneway() async oneway {}

  // A plain distributed void method: isOnewayRemoteCall stays false.
  distributed func plainVoid() {}
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()

    // A local actor whose system always resolves references as remote, so calls
    // drive the remote branch of the synthesized thunk. That branch invokes
    // remoteCallVoid; for 'oneway' targets the target's isOnewayRemoteCall flag
    // is set, which the system observes and reports.
    let local = Greeter(actorSystem: system)
    let ref = try Greeter.resolve(id: local.id, using: system)

    // ==== Oneway void method carries the flag.
    try await ref.thanksOneway()
    // CHECK: >> remoteCallVoid: is oneway call

    // ==== Oneway async void method carries the flag.
    try await ref.pingOneway()
    // CHECK: >> remoteCallVoid: is oneway call

    // ==== Plain distributed void method does NOT report the flag.
    try await ref.plainVoid()
    // CHECK-NOT: is oneway call

    print("done")
    // CHECK: done
  }
}
