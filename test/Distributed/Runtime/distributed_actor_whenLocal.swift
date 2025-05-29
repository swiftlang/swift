// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main  -target %target-swift-5.7-abi-triple -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
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

distributed actor Capybara {
  // only the local capybara can do this!
  func eat() -> String {
    "watermelon"
  }
}

typealias DefaultDistributedActorSystem = FakeActorSystem

func test() async throws {
  let system = DefaultDistributedActorSystem()

  let local = Capybara(actorSystem: system)
  // await local.eat() // SHOULD ERROR
  let valueWhenLocal: String? = await local.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.eat()
  }

  // CHECK: valueWhenLocal: watermelon
  print("valueWhenLocal: \(valueWhenLocal ?? "nil")")

  let remote = try Capybara.resolve(id: local.id, using: system)
  let valueWhenRemote: String? = await remote.whenLocal { __secretlyKnownToBeLocal in
    __secretlyKnownToBeLocal.eat()
  }
  
  // CHECK: valueWhenRemote: nil
  print("valueWhenRemote: \(valueWhenRemote ?? "nil")")
}

@main struct Main {
  static func main() async {
    try! await test()
  }
}
