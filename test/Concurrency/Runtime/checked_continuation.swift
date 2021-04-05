// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency)

// REQUIRES: executable_test
// REQUIRES: concurrency

import _Concurrency
import StdlibUnittest

struct TestError: Error {}

var tests = TestSuite("CheckedContinuation")

if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
  tests.test("trap on double resume non-throwing continuation") {
    expectCrashLater()
    runAsyncAndBlock {
      let _: Int = await withCheckedContinuation { c in
        c.resume(returning: 17)
        c.resume(returning: 38)
      }
    }
  }

  tests.test("trap on double resume throwing continuation") {
    expectCrashLater()
    runAsyncAndBlock {
      do {
        let _: Int = try await withCheckedThrowingContinuation { c in
          c.resume(returning: 17)
          c.resume(throwing: TestError())
        }
      } catch {
      }
    }
  }
}

runAllTests()
