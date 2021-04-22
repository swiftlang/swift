// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency -parse-as-library)

// REQUIRES: executable_test
// REQUIRES: concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import _Concurrency
import StdlibUnittest

struct TestError: Error {}

@main struct Main {
  static func main() async {
    var tests = TestSuite("CheckedContinuation")

    if #available(macOS 9999, iOS 9999, watchOS 9999, tvOS 9999, *) {
      tests.test("trap on double resume non-throwing continuation") {
        expectCrashLater()

        let task = detach {
          let _: Int = await withCheckedContinuation { c in
            c.resume(returning: 17)
            c.resume(returning: 38)
          }
        }
        await task.get()
      }

      tests.test("trap on double resume throwing continuation") {
        expectCrashLater()

        let task = detach {
          do {
            let _: Int = try await withCheckedThrowingContinuation { c in
              c.resume(returning: 17)
              c.resume(throwing: TestError())
            }
          } catch {
          }
        }
        await task.get()
      }
    }

    await runAllTestsAsync()
  }
}
