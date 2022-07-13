// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -disable-availability-checking %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main -Xfrontend -disable-availability-checking -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
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

distributed actor Greeter {
  deinit {
      print("DEINIT \(self.id)")
  }
}

func test() async throws {
  let system = DefaultDistributedActorSystem()

  var local: Greeter? = Greeter(actorSystem: system)
  var ref: Greeter? = try Greeter.resolve(id: local!.id, using: system)
  assert(__isRemoteActor(ref!))

  local = nil
  ref = nil

  print("DONE")
  // CHECK: DEINIT
  // CHECK-NEXT: DONE

}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
