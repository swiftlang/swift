// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime

// TODO: The actual reason is that we do these %env- tricks, which e.g. Windows is confused about
// REQUIRES: libdispatch

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest

func checkAssumeMainActor(echo: MainActorEcho) /* synchronous! */ {
  // Echo.get("any") // error: main actor isolated, cannot perform async call here
  MainActor.assumeIsolated {
    let input = "example"
    let got = echo.get(input)
    precondition(got == "example", "Expected echo to match \(input)")
  }
}

@MainActor
func mainActorCallCheck(echo: MainActorEcho) {
  checkAssumeMainActor(echo: echo)
}

actor MainFriend {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    MainActor.sharedUnownedExecutor
  }

  func callCheck(echo: MainActorEcho) {
    checkAssumeMainActor(echo: echo)
  }
}

func checkAssumeSomeone(someone: SomeoneOnDefaultExecutor) /* synchronous */ {
  // someone.something // can't access, would need a hop but we can't
  someone.assumeIsolated { someone in
    let something = someone.something
    let expected = "isolated something"
    precondition(something == expected, "expected '\(expected)', got: \(something)")
  }
}

actor SomeoneOnDefaultExecutor {
  func callCheckMainActor(echo: MainActorEcho) {
    checkAssumeMainActor(echo: echo)
  }

  func callCheckSomeone() {
    checkAssumeSomeone(someone: self)
  }

  var something: String {
    "isolated something"
  }
}

actor SomeonesFriend {
  let someone: SomeoneOnDefaultExecutor
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.someone.unownedExecutor
  }

  init(someone: SomeoneOnDefaultExecutor) {
    self.someone = someone
  }

  func callCheckSomeone() {
    checkAssumeSomeone(someone: someone)
  }
}

actor CompleteStranger {
  let someone: SomeoneOnDefaultExecutor
  init(someone: SomeoneOnDefaultExecutor) {
    self.someone = someone
  }

  func callCheckSomeone() {
    checkAssumeSomeone(someone: someone)
  }
}

@MainActor
final class MainActorEcho {
  func get(_ key: String) -> String {
    key
  }
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssumeActorExecutor")

    let echo = MainActorEcho()

    if #available(SwiftStdlib 5.1, *) {
      // === MainActor --------------------------------------------------------

      tests.test("MainActor.assumeIsolated: assume the main executor, from 'main() async'") {
        await checkAssumeMainActor(echo: echo)
      }

      tests.test("MainActor.assumeIsolated: assume the main executor, from MainActor method") {
        await mainActorCallCheck(echo: echo)
      }

      tests.test("MainActor.assumeIsolated: assume the main executor, from actor on MainActor executor") {
        await MainFriend().callCheck(echo: echo)
      }

      #if !os(WASI)
      tests.test("MainActor.assumeIsolated: wrongly assume the main executor, from actor on other executor") {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected 'MainActor' executor.")
        await SomeoneOnDefaultExecutor().callCheckMainActor(echo: echo)
      }
      #endif

      // === some Actor -------------------------------------------------------

      let someone = SomeoneOnDefaultExecutor()
      #if !os(WASI)
      tests.test("assumeOnActorExecutor: wrongly assume someone's executor, from 'main() async'") {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected same executor as a.SomeoneOnDefaultExecutor.")
        checkAssumeSomeone(someone: someone)
      }

      tests.test("assumeOnActorExecutor: wrongly assume someone's executor, from MainActor method") {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected same executor as a.SomeoneOnDefaultExecutor.")
        checkAssumeSomeone(someone: someone)
      }
      #endif

      tests.test("assumeOnActorExecutor: assume someone's executor, from SomeoneOnDefaultExecutor") {
        await someone.callCheckSomeone()
      }

      tests.test("assumeOnActorExecutor: assume someone's executor, from actor on the SomeoneOnDefaultExecutor.unownedExecutor") {
        await SomeonesFriend(someone: someone).callCheckSomeone()
      }

      #if !os(WASI)
      tests.test("assumeOnActorExecutor: wrongly assume the main executor, from actor on other executor") {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected same executor as a.SomeoneOnDefaultExecutor.")
        await CompleteStranger(someone: someone).callCheckSomeone()
      }
      #endif

    }

    await runAllTestsAsync()
  }
}
