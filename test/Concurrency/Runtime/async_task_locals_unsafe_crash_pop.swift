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

enum TL {

  @TaskLocal
  static var number: Int = 0

  @TaskLocal
  static var other: String = ""
}

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssertPreconditionActorExecutor")

    tests.test("pop more task-local bindings than were set") {
      expectCrashLater(withMessage:
        "Attempted to pop task-local value binding from empty task-local bindings stack. This indicates an un-balanced number of 'unsafePushValue' and 'unsafePopValue' calls.")
      TL.$number.unsafePushValue(1)
      TL.$number.unsafePopValue()
      TL.$number.unsafePopValue() // boom!
    }

    tests.test("illegal pop from parent") {
      expectCrashLater(withMessage:
        "freed pointer was not the last allocation")
      TL.$number.unsafePushValue(1)
      _ = await Task {
        TL.$number.unsafePushValue(2)
        TL.$number.unsafePopValue()
        TL.$number.unsafePopValue() // BAD ALREADY! tries to pop in parent!
      }.value
    }

    tests.test("pop unexpected key") {
      expectCrashLater(withMessage:
        "unsafePopValue key did not match actually removed binding. This a indicates not well-balanced push/pop pair of calls. Expected key TaskLocal<Int>")
      TL.$number.unsafePushValue(1)
      TL.$other.unsafePushValue("hi")
      // expected order of popping should be reverse of pushing
      TL.$number.unsafePopValue()
    }

    await runAllTestsAsync()
  }
}
