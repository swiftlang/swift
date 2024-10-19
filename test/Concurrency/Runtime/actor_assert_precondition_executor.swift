// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// TODO: The actual reason is that we do these %env- tricks, which e.g. Windows is confused about
// REQUIRES: libdispatch

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

// rdar://119743909 fails in optimize tests.
// UNSUPPORTED: swift_test_mode_optimize
// UNSUPPORTED: swift_test_mode_optimize_size

import StdlibUnittest

func checkPreconditionMainActor() /* synchronous! */ {
  MainActor.shared.preconditionIsolated()
  MainActor.preconditionIsolated()

  // check for the existence of the assert version of APIs
  MainActor.shared.assertIsolated()
  MainActor.assertIsolated()
}

func checkPreconditionFamousActor() /* synchronous! */ {
  FamousActor.shared.preconditionIsolated() // instance version for global actor
  FamousActor.preconditionIsolated() // static version for global actor
}

@MainActor
func mainActorCallCheck() {
  checkPreconditionMainActor()
}

@globalActor actor FamousActor: GlobalActor {
  public static let shared = FamousActor()

  func callCheckFamousActor() {
    checkPreconditionFamousActor() // ok
  }

  func callCheckMainActor() {
    checkPreconditionMainActor() // bad
  }
}

actor MainFriend {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    MainActor.sharedUnownedExecutor
  }

  func callCheckMainActor() {
    checkPreconditionMainActor()
  }
}

actor Someone {
  func callCheckMainActor() {
    checkPreconditionMainActor()
  }
}

@MainActor let global = TestStaticVar()

@MainActor
struct TestStaticVar {
  @MainActor static let shared = TestStaticVar()

  init() {
    checkPreconditionMainActor()
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssertPreconditionActorExecutor")

    if #available(SwiftStdlib 5.9, *) {
      // === MainActor --------------------------------------------------------

      tests.test("precondition on actor (main): from 'main() async', with await") {
        await checkPreconditionMainActor()
      }

      // FIXME: calling without await from main() should also succeed, we must set the executor while we're kicking off main

      tests.test("MainActor.preconditionIsolated(): from Main friend") {
        await MainFriend().callCheckMainActor()
      }

      tests.test("MainActor.assertIsolated() from static let initializer") {
        _ = await TestStaticVar.shared
        _ = await global
      }

      #if !os(WASI)
      tests.test("precondition on actor (main): wrongly assume the main executor, from actor on other executor") {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected 'MainActor' executor.")
        await Someone().callCheckMainActor()
      }
      #endif

      // === Global actor -----------------------------------------------------

      tests.test("precondition on actor (main): assume FamousActor, from FamousActor") {
        await FamousActor.shared.callCheckFamousActor()
      }


    }

    await runAllTestsAsync()
  }
}
