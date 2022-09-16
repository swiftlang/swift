// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s --color

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: windows

import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeActorSystem

protocol CallMe {
  func maybe() async throws
}

distributed actor Lucy: CallMe {
  distributed func maybe() {
    precondition(__isLocalActor(self), "A distributed func body may only ever execute in a real local instance!")
    print("Didn't think you'd call")
  }
}

@main struct Main {
  static func main() async {
    var system = DefaultDistributedActorSystem()
    system.throwRemoteCallVoid = false // just return () from remoteCallVoid

    let id = ActorAddress(parse: "")
    let callMe: any CallMe = try! Lucy.resolve(id: id, using: system)
    try! await callMe.maybe()

    // CHECK-NOT: Didn't think you'd call

    print("OK") // CHECK: OK
  }
}
