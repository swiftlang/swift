// RUN: %target-run-simple-swift( -Xfrontend -disable-availability-checking -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import Dispatch
import StdlibUnittest

actor EscapeLocked {
  var k: Int = 1
  
  func increment() {
    k += 1
  }
  
  deinit {
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
    let tests = TestSuite("EscapingSelf")
    tests.test("escape while locked") {
      _ = EscapeLocked()
    }
    await runAllTestsAsync()
  }
}
