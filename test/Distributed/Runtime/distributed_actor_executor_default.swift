// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.9-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -target %target-swift-5.9-abi-triple -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: distributed
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest
import Distributed
import FakeDistributedActorSystems

typealias DefaultDistributedActorSystem = FakeRoundtripActorSystem

@available(SwiftStdlib 5.9, *)
distributed actor DefaultDistributedActor {
  distributed func test(x: Int) async throws {
    print("executed: \(#function)")
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("DistributedActorDefaultExecutorTests")

    let system = FakeRoundtripActorSystem()
    let distLocal = DefaultDistributedActor(actorSystem: system)

    if #available(SwiftStdlib 5.9, *) {

      tests.test("distributed actor, local: default executor, just works") {
        try! await distLocal.test(x: 42)
      }

      tests.test("distributed actor, remote: obtaining executor works") {
        let remoteRef = try! DefaultDistributedActor.resolve(id: distLocal.id, using: system)
        print("Executor was: \(remoteRef.unownedExecutor)") // accessing it is okey, it will be the "explode on use" executor
      }
    }

    await runAllTestsAsync()
  }
}
