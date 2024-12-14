// RUN: %target-run-simple-swift( -target %target-future-triple %import-libdispatch -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: libdispatch
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import Dispatch
import StdlibUnittest

func formatAddr(_ obj: AnyObject) -> String {
  "0x" + String(unsafeBitCast(obj, to: UInt.self), radix: 16)
}

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
    expectCrashLater(withMessage: "Object \(formatAddr(self)) of class EscapeLocked deallocated with non-zero retain count 2. This object's deinit, or something called from it, may have created a strong reference to self which outlived deinit, resulting in a dangling reference.")
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
