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
  @remoteCall(oneway)
  distributed func thanksOneway() {} // remoteCallVoid

  distributed func plainVoid() {}
}

protocol OnewayGreeterProtocol: DistributedActor
    where ActorSystem: DistributedActorSystem<any Codable> {
  @remoteCall(oneway)
  distributed func inheritedOneway()
}

distributed actor InheritingGreeter: OnewayGreeterProtocol {
  distributed func inheritedOneway() {}
}

@main struct Main {
  static func main() async throws {
    let system = FakeRoundtripActorSystem()

    let local = Greeter(actorSystem: system)
    let ref = try Greeter.resolve(id: local.id, using: system)

    // ==== Oneway void method carries the flag.
    try await ref.thanksOneway()
    // CHECK: >> remoteCallVoid: is oneway call

    // ==== Witness inheriting 'oneway' from its protocol requirement
    let localInheriting = InheritingGreeter(actorSystem: system)
    let refInheriting = try InheritingGreeter.resolve(id: localInheriting.id, using: system)
    try await refInheriting.inheritedOneway()
    // CHECK: >> remoteCallVoid: is oneway call

    // ==== Plain distributed void method does NOT report the flag.
    try await ref.plainVoid()
    // CHECK-NOT: is oneway call

    print("done")
    // CHECK: done
  }
}
