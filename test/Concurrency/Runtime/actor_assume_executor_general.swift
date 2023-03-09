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

func checkAssumeMainActor(echo: MainActorEcho) /* synchronous! */ {
  // Echo.get("any") // error: main actor isolated, cannot perform async call here
  assumeOnMainActorExecutor {
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

func checkAssumeDefaultExecutorSomeone(someone: DefaultExecutorSomeone) /* synchronous */ {
  // someone.something // can't access, would need a hop but we can't
  assumeOnActorExecutor(someone) { someone in
    let something = someone.something
    let expected = "isolated something"
    precondition(something == expected, "expected '\(expected)', got: \(something)")
  }
}

actor DefaultExecutorSomeone {
  func callCheckMainActor(echo: MainActorEcho) {
    checkAssumeMainActor(echo: echo)
  }

  func callCheckDefaultExecutorSomeone() {
    checkAssumeDefaultExecutorSomeone(someone: self)
  }

  var something: String {
    "isolated something"
  }
}

actor DefaultExecutorSomeonesFriend {
  let someone: DefaultExecutorSomeone
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    self.someone.unownedExecutor
  }

  init(someone: DefaultExecutorSomeone) {
    self.someone = someone
  }

  func callCheckDefaultExecutorSomeone() {
    checkAssumeDefaultExecutorSomeone(someone: someone)
  }
}

final class InlineExecutor: SerialExecutor {
  public func enqueue(_ job: UnownedJob) {
    print("\(self): enqueue")
    job.runSynchronously(on: self.asUnownedSerialExecutor())
    print("\(self): after run")
  }

  public func asUnownedSerialExecutor() -> UnownedSerialExecutor {
    return UnownedSerialExecutor(ordinary: self)
  }
}

let inlineExecutor = InlineExecutor()

actor InlineExecutorActor {
  let someone: DefaultExecutorSomeone

  // FIXME: why is making this a let cause crashes in swift_task_enqueueImpl
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    inlineExecutor.asUnownedSerialExecutor()
  }

  init(someone: DefaultExecutorSomeone) {
    self.someone = someone
  }

  func callCheckDefaultExecutorSomeone() {
    checkAssumeDefaultExecutorSomeone(someone: someone)
  }

  func checkMainActor() {
    preconditionTaskOnActorExecutor(MainActor.shared)
  }
}

@MainActor
final class MainActorEcho {
  func get(_ key: String) -> String {
    key
  }
}

/// Crash tests
@main struct Main {
  static func main() async {
    let tests = TestSuite("AssumeActorExecutor")
    let echo = MainActorEcho()

    if #available(SwiftStdlib 5.9, *) {
      // === MainActor --------------------------------------------------------

      tests.test("assumeOnMainActorExecutor: assume the main executor, from 'main() async'") {
        await checkAssumeMainActor(echo: echo) // purposefully await here, forcing a hop to MainActor
      }

      tests.test("assumeOnMainActorExecutor: assume the main executor, from MainActor method") {
        await mainActorCallCheck(echo: echo)
      }

      tests.test("assumeOnMainActorExecutor: assume the main executor, from actor on MainActor executor") {
        await MainFriend().callCheck(echo: echo)
      }

      tests.test("assumeOnMainActorExecutor: wrongly assume the main executor, from actor on default executor") {
        // Note that the executor here is the type of the actor; "default" executors are per actor
        expectCrashLater(withMessage: "Expected 'MainActorExecutor' executor, but was executing on 'a.DefaultExecutorSomeone'.")
        await DefaultExecutorSomeone().callCheckMainActor(echo: echo)
      }

      // === some Actor -------------------------------------------------------

      let someone = DefaultExecutorSomeone()

      tests.test("assumeOnActorExecutor: wrongly assume someone's executor, from 'main() async'") {
        expectCrashLater(withMessage: "Expected same executor as actor 'a.DefaultExecutorSomeone' ('DefaultActorExecutor'), but was executing on 'GenericExecutor'.")
        checkAssumeDefaultExecutorSomeone(someone: someone)
      }

      tests.test("assumeOnActorExecutor: wrongly assume someone's executor, from MainActor method") {
        expectCrashLater(withMessage: "Expected same executor as actor 'a.DefaultExecutorSomeone' ('DefaultActorExecutor'), but was executing on 'GenericExecutor'.")
        checkAssumeDefaultExecutorSomeone(someone: someone)
      }

      tests.test("assumeOnActorExecutor: assume someone's executor, from DefaultExecutorSomeone") {
        await someone.callCheckDefaultExecutorSomeone()
      }

      tests.test("assumeOnActorExecutor: assume someone's executor, from actor on the DefaultExecutorSomeone.unownedExecutor") {
        await DefaultExecutorSomeonesFriend(someone: someone).callCheckDefaultExecutorSomeone()
      }

      tests.test("assumeOnActorExecutor: wrongly assume the main executor, from actor on other executor") {
        expectCrashLater(withMessage: "Expected same executor as actor 'a.DefaultExecutorSomeone' ('DefaultActorExecutor'), but was executing on 'a.InlineExecutor'.")
        await InlineExecutorActor(someone: someone).callCheckDefaultExecutorSomeone()
      }

      tests.test("assumeOnActorExecutor: wrongly assume the main executor, on custom executor") {
        expectCrashLater(withMessage: "Expected same executor as actor 'Swift.MainActor' ('MainActorExecutor'), but was executing on 'a.InlineExecutor'.")
        await InlineExecutorActor(someone: someone).checkMainActor()
      }
    }

    await runAllTestsAsync()
  }
}
