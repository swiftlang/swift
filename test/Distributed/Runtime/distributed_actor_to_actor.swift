// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -module-name main %import-libdispatch -j2 -parse-as-library -Xfrontend -disable-availability-checking -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -g -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: libdispatch

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// FIXME(distributed): Distributed actors currently have some issues on windows rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import Dispatch
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

distributed actor Worker {
  var counter: Int = 0

  distributed func test() async {
    print("Starting on distributed actor \(self)")
    counter = counter + 1
    let result = await asLocalActor.doSomethingAsync {
      incrementCounter()
      return "\(counter)"
    }
    print("Distributed actor received \(result)")
  }

  func incrementCounter() {
    counter += 1
  }
}

extension Actor {
  func doSomethingAsync(body: () async -> String) async -> String {
    print("Executing on local actor \(self)")
    let result = await body()
    print("Got \(result) back")
    return result + "!"
  }
}

@main struct Main {
  static func main() async throws {
    let worker = Worker(actorSystem: DefaultDistributedActorSystem())

    // CHECK: Starting on distributed actor [[PTR:.*]]
    // CHECK: Executing on local actor [[PTR]]
    // CHECK: Got 2 back
    // CHECK: Distributed actor received 2!
    try await worker.test()

    print("OK") // CHECK: OK
  }
}
