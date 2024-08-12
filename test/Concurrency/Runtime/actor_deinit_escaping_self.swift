// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: libdispatch
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: swift_stdlib_asserts
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import Dispatch
import StdlibUnittest

actor EscapeLocked {
  var k: Int = 1
  
  func increment() {
    k += 1
  }
  
  isolated deinit {
    let g = DispatchGroup()
    g.enter()
    Task.detached {
      await self.increment()
      g.leave()
    }
    let r = g.wait(timeout: .now() + .milliseconds(500))
    expectEqual(r, .timedOut)
    expectCrashLater(withMessage: "Assertion failed: (!oldState.getFirstUnprioritisedJob() && \"actor has queued jobs at destruction\"), function destroy")
  }
}

@main struct Main {
  static func main() async {
    // Ideally these tests should be compile-time errors
    let tests = TestSuite("EscapingSelf")
    tests.test("escape while locked") {
      _ = EscapeLocked()
    }
    await runAllTestsAsync()
  }
}
