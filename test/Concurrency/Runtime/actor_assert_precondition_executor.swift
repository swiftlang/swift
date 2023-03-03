// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest

func checkPreconditionMainActor() /* synchronous! */ {
  preconditionTaskOnActorExecutor(MainActor.shared)
}

func checkPreconditionFamousActor() /* synchronous! */ {
  preconditionTaskOnActorExecutor(FamousActor.shared)
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

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssertPreconditionActorExecutor")

    if #available(SwiftStdlib 5.9, *) {
      // === MainActor --------------------------------------------------------

      tests.test("preconditionTaskOnActorExecutor(main): from 'main() async', with await") {
        await checkPreconditionMainActor()
      }

      // FIXME: calling without await from main() should also succeed, we must set the executor while we're kicking off main

      tests.test("preconditionTaskOnActorExecutor(main): from Main friend") {
        await MainFriend().callCheckMainActor()
      }

      tests.test("preconditionTaskOnActorExecutor(main): wrongly assume the main executor, from actor on other executor") {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected 'MainActor' executor.")
        await Someone().callCheckMainActor()
      }

      // === Global actor -----------------------------------------------------

      tests.test("preconditionTaskOnActorExecutor(main): assume FamousActor, from FamousActor") {
        await FamousActor.shared.callCheckFamousActor()
      }


    }

    await runAllTestsAsync()
  }
}