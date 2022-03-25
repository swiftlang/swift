// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
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

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol LifecycleWatch: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
}

extension LifecycleWatch {
  func watch() async throws {
    // nothing here
    print("executed: \(#function)")
  }

  distributed func test() async throws {
    print("executed: \(#function)")
    try await self.watch()
    print("done executed: \(#function)")
  }
}

distributed actor Worker: LifecycleWatch {
}

@main struct Main {
  static func main() async {
    let worker: any LifecycleWatch = Worker(actorSystem: DefaultDistributedActorSystem())
    try! await worker.test()

    // CHECK: executed: test()
    // CHECK: executed: watch()
    // CHECK: done executed: test()

    print("OK") // CHECK: OK
  }
}
