// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -enable-experimental-distributed -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows, isRemote always returns false. rdar://82593574
// UNSUPPORTED: OS=windows-msvc


import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

protocol LifecycleWatch: DistributedActor where ActorSystem == FakeRoundtripActorSystem {
  func terminated(actor id: ID) async
}

extension LifecycleWatch {
  func watch<T: Codable>(x: Int, _ y: T) async throws {
    // nothing here
    print("executed: \(#function) - x = \(x), y = \(y)")
  }

  distributed func test<T: Codable & Sendable>(x: Int, _ y: T) async throws {
    print("executed: \(#function)")
    try await self.watch(x: x, y)
    print("done executed: \(#function)")
  }
}

distributed actor Worker: LifecycleWatch {
  func terminated(actor id: ID) async {
    print("terminated (on \(self.id)): \(id)")
  }
}

@main struct Main {
  static func main() async {
    let worker: any LifecycleWatch = Worker(actorSystem: DefaultDistributedActorSystem())
    try! await worker.test(x: 42, "on protocol")

    // CHECK: executed: test(x:_:)
    // CHECK: executed: watch(x:_:) - x = 42, y = on protocol
    // CHECK: done executed: test(x:_:)

    // FIXME: Actor isolation getting with generics is pending implementation #59356
    do {
      let terminatedID = Worker.ID(parse: "<terminated-id>")
      let __secretlyKnownToBeLocal = worker
      await __secretlyKnownToBeLocal.terminated(actor: terminatedID)

      // FIXME: Once the above fixme is solved, use this real code instead:
      //    _ = await worker.whenLocal { __secretlyKnownToBeLocal in
      //      let terminatedID = Worker.ID(parse: "<terminated-id>")
      //      return await __secretlyKnownToBeLocal.terminated(actor: terminatedID)
      //    }
    }
    // CHECK: terminated (on ActorAddress(address: "<unique-id>")): ActorAddress(address: "<terminated-id>")

    print("OK") // CHECK: OK
  }
}
