// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/a.out

// 'GlobalActor.assumeIsolated' for any global actor, which needs '@isolated(Self)'
// RUN: %if swift_feature_AllowIsolatedGlobalActorAttribute %{ %target-build-swift -target %target-swift-5.1-abi-triple -parse-as-library -enable-experimental-feature AllowIsolatedGlobalActorAttribute %s -o %t/a-isolated-global-actor.out %}
// RUN: %if swift_feature_AllowIsolatedGlobalActorAttribute %{ %target-codesign %t/a-isolated-global-actor.out %}
// RUN: %if swift_feature_AllowIsolatedGlobalActorAttribute %{ env %env-SWIFT_IS_CURRENT_EXECUTOR_LEGACY_MODE_OVERRIDE=legacy %target-run %t/a-isolated-global-actor.out %}

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
  // Echo.get("any") // error: MainActor isolated, cannot perform async call here
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

#if $AllowIsolatedGlobalActorAttribute
@globalActor
actor MyGlobalActor {
  static let shared = MyGlobalActor()
}

@MyGlobalActor var myGlobalActorCounter = 0

func checkAssumeMyGlobalActor() -> Int /* synchronous! */ {
  MyGlobalActor.assumeIsolated {
    myGlobalActorCounter += 1
    return myGlobalActorCounter
  }
}

func checkAssumeGeneric<G: GlobalActor>(_: G.Type) -> String /* synchronous! */ {
  G.assumeIsolated {
    "assumed \(G.self)"
  }
}

struct AssumeError: Error {}

func checkAssumeGenericThrows<G: GlobalActor>(_: G.Type) throws(AssumeError) {
  try G.assumeIsolated { () throws(AssumeError) in
    throw AssumeError()
  }
}

@MyGlobalActor
func myGlobalActorCallCheck() -> Int {
  checkAssumeMyGlobalActor()
}

@MyGlobalActor
func myGlobalActorGenericCallCheck() -> String {
  checkAssumeGeneric(MyGlobalActor.self)
}

@MainActor
func mainActorGenericCallCheck() -> String {
  checkAssumeGeneric(MainActor.self)
}

@MainActor
func mainActorCallCheckGenericOther() -> String {
  checkAssumeGeneric(MyGlobalActor.self)
}

@MyGlobalActor
func myGlobalActorThrowsCallCheck() {
  do {
    try checkAssumeGenericThrows(MyGlobalActor.self)
    expectUnreachable("expected an error to be thrown")
  } catch {
    // expected
  }
}

actor MyGlobalActorFriend {
  nonisolated var unownedExecutor: UnownedSerialExecutor {
    MyGlobalActor.sharedUnownedExecutor
  }

  func callCheck() -> Int {
    checkAssumeMyGlobalActor()
  }
}

extension SomeoneOnDefaultExecutor {
  func callCheckMyGlobalActor() -> Int {
    checkAssumeMyGlobalActor()
  }
}

func hopToGeneric<G: GlobalActor>(
  _: G.Type, _ fn: @escaping @isolated(G) () -> Int
) async -> Int {
  await fn()
}

func erase<G: GlobalActor>(
  _: G.Type, _ fn: @escaping @isolated(G) () -> Int
) -> Any {
  fn
}
#endif

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

      tests.test("MainActor.assumeIsolated: wrongly assume the main executor, from actor on other executor")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected 'MainActor' executor.")
        await SomeoneOnDefaultExecutor().callCheckMainActor(echo: echo)
      }

      // === some Actor -------------------------------------------------------

      let someone = SomeoneOnDefaultExecutor()
      tests.test("assumeOnActorExecutor: wrongly assume someone's executor, from 'main() async'")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected same executor as a.SomeoneOnDefaultExecutor.")
        checkAssumeSomeone(someone: someone)
      }

      tests.test("assumeOnActorExecutor: wrongly assume someone's executor, from MainActor method")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected same executor as a.SomeoneOnDefaultExecutor.")
        checkAssumeSomeone(someone: someone)
      }

      tests.test("assumeOnActorExecutor: assume someone's executor, from SomeoneOnDefaultExecutor") {
        await someone.callCheckSomeone()
      }

      tests.test("assumeOnActorExecutor: assume someone's executor, from actor on the SomeoneOnDefaultExecutor.unownedExecutor") {
        await SomeonesFriend(someone: someone).callCheckSomeone()
      }

      tests.test("assumeOnActorExecutor: wrongly assume the main executor, from actor on other executor")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption; Expected same executor as a.SomeoneOnDefaultExecutor.")
        await CompleteStranger(someone: someone).callCheckSomeone()
      }

#if $AllowIsolatedGlobalActorAttribute
      // === any GlobalActor --------------------------------------------------

      tests.test("GlobalActor.assumeIsolated: assume a custom global actor, from its isolated function") {
        _ = await myGlobalActorCallCheck()
      }

      tests.test("GlobalActor.assumeIsolated: assume a custom global actor, from actor on its executor") {
        _ = await MyGlobalActorFriend().callCheck()
      }

      tests.test("GlobalActor.assumeIsolated: assume a generic global actor") {
        expectEqual("assumed MyGlobalActor", await myGlobalActorGenericCallCheck())
        expectEqual("assumed MainActor", await mainActorGenericCallCheck())
      }

      tests.test("GlobalActor.assumeIsolated: rethrow the typed error of the operation") {
        await myGlobalActorThrowsCallCheck()
      }

      tests.test("GlobalActor.assumeIsolated: wrongly assume a custom global actor, from 'main() async'")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption")
        _ = checkAssumeMyGlobalActor()
      }

      tests.test("GlobalActor.assumeIsolated: wrongly assume a custom global actor, from actor on other executor")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption")
        _ = await SomeoneOnDefaultExecutor().callCheckMyGlobalActor()
      }

      tests.test("GlobalActor.assumeIsolated: wrongly assume a generic global actor")
      .require(.crashTesting)
      .code {
        expectCrashLater(withMessage: "Incorrect actor executor assumption")
        _ = await mainActorCallCheckGenericOther()
      }

      // === '@isolated(G)' function types ------------------------------------

      tests.test("@isolated(G): calling hops to the generic global actor") {
        let value = await hopToGeneric(MyGlobalActor.self) {
          MyGlobalActor.shared.preconditionIsolated()
          return 42
        }
        expectEqual(42, value)
      }

      tests.test("@isolated(G): function type metadata records the global actor") {
        let erased = erase(MyGlobalActor.self) { @MyGlobalActor in 7 }
        expectTrue(erased is @MyGlobalActor () -> Int)
        expectFalse(erased is () -> Int)
      }
#endif
    }

    await runAllTestsAsync()
  }
}
