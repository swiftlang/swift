// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend-emit-module -emit-module-path %t/FakeDistributedActorSystems.swiftmodule -module-name FakeDistributedActorSystems %S/../Inputs/FakeDistributedActorSystems.swift
// RUN: %target-build-swift -parse-as-library -I %t %s %S/../Inputs/FakeDistributedActorSystems.swift -o %t/a.out
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

@available(SwiftStdlib 5.7, *)
typealias DefaultDistributedActorSystem = LocalTestingDistributedActorSystem

@available(SwiftStdlib 5.7, *)
distributed actor FiveSevenActor_NothingExecutor {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  distributed func test(x: Int) async throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    MainActor.assumeIsolated {
      // ignore
    }
  }
}

@available(SwiftStdlib 5.9, *)
distributed actor FiveNineActor_NothingExecutor {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  distributed func test(x: Int) async throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    MainActor.assumeIsolated {
      // ignore
    }
  }
}

@available(SwiftStdlib 5.7, *)
distributed actor FiveSevenActor_FiveNineExecutor {
  @available(SwiftStdlib 5.9, *)
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    print("get unowned executor")
    return MainActor.sharedUnownedExecutor
  }

  distributed func test(x: Int) async throws {
    print("executed: \(#function)")
    defer {
      print("done executed: \(#function)")
    }
    MainActor.assumeIsolated {
      // ignore
    }
  }
}

@available(SwiftStdlib 5.7, *)
@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      let tests = TestSuite("DistributedActorExecutorAvailability")

      let system = LocalTestingDistributedActorSystem()

      #if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
      tests.test("5.7 actor, no availability executor property => no custom executor") {
        expectCrashLater(withMessage: "Fatal error: Incorrect actor executor assumption; Expected 'MainActor' executor.")
        try! await FiveSevenActor_NothingExecutor(actorSystem: system).test(x: 42)
      }

      tests.test("5.9 actor, no availability executor property => custom executor") {
        try! await FiveNineActor_NothingExecutor(actorSystem: system).test(x: 42)
      }

      tests.test("5.7 actor, 5.9 executor property => no custom executor") {
        expectCrashLater(withMessage: "Fatal error: Incorrect actor executor assumption; Expected 'MainActor' executor.")
        try! await FiveSevenActor_FiveNineExecutor(actorSystem: system).test(x: 42)
      }
      #else
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
      #endif

      await runAllTestsAsync()
    }
  }
}
