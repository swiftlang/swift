// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems -target %target-swift-5.9-abi-triple %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -parse-as-library -target %target-swift-5.9-abi-triple -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift %S/../Inputs/CustomSerialExecutorAvailability.swift -o %t/a.out
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

@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let tests = TestSuite("DistributedActorExecutorAvailabilitySwift59")

      let system = LocalTestingDistributedActorSystem()

      // On non-apple platforms the SDK comes with the toolchains,
      // so the feature works because we're executing in a 5.9 context already,
      // which otherwise could not have been compiled
      tests.test("non apple platform: 5.7 actor, no availability executor property => no custom executor") {
        try! await FiveSevenActor_NothingExecutor(actorSystem: system).test(x: 42)
      }

      tests.test("non apple platform: 5.9 actor, no availability executor property => custom executor") {
        try! await FiveNineActor_NothingExecutor(actorSystem: system).test(x: 42)
      }

      tests.test("non apple platform: 5.7 actor, 5.9 executor property => no custom executor") {
        try! await FiveSevenActor_FiveNineExecutor(actorSystem: system).test(x: 42)
      }

      await runAllTestsAsync()
    }
  }
}
