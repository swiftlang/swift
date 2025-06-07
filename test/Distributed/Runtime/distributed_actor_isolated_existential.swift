// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.7-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -target %target-swift-5.7-abi-triple -module-name main -j2 -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift  -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed

// rdar://76038845
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: freestanding

// FIXME(distributed): Distributed actors currently have some issues on windows rdar://82593574
// UNSUPPORTED: OS=windows-msvc

import StdlibUnittest
import Distributed
import FakeDistributedActorSystems

@available(SwiftStdlib 5.9, *)
typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@available(SwiftStdlib 5.9, *)
func globalFunc(isolatedTo actor: isolated any DistributedActor) async {
  MainActor.preconditionIsolated() // we forced the `actor` onto the main actor's executor
}

@available(SwiftStdlib 5.9, *) // because conforming to the protocol requirement introduced in 5.9
distributed actor NormalWorker {

  nonisolated var unownedExecutor: UnownedSerialExecutor {
    return MainActor.sharedUnownedExecutor
  }

  distributed func offerSelf() async {
    print("executed: \(#function)")
    MainActor.preconditionIsolated() // we forced the `actor` onto the main actor's executor

    await globalFunc(isolatedTo: self)
    MainActor.preconditionIsolated() // we forced the `actor` onto the main actor's executor
  }
}

@available(SwiftStdlib 5.1, *)
@main struct Main {
  @available(SwiftStdlib 5.1, *)
  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let tests = TestSuite("DistributedIsolatedTests")
      let system = DefaultDistributedActorSystem()
      let normalLocalWorker = NormalWorker(actorSystem: system)
      precondition(__isLocalActor(normalLocalWorker), "must be local")

      tests.test("remote actor reference should have crash-on-enqueue executor") {
        try! await normalLocalWorker.offerSelf()
      }

      await runAllTestsAsync()
    }
  }
}
