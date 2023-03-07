// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking -parse-as-library %s -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN:  %target-run %t/a.out

// REQUIRES: executable_test
// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: libdispatch
// UNSUPPORTED: back_deployment_runtime

// UNSUPPORTED: back_deploy_concurrency
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: freestanding

import StdlibUnittest
import Dispatch

@main struct Main {
  static func main() async {
    let tests = TestSuite("AssumeActorExecutor")
    let q = DispatchQueue(label: "queue")

    if #available(SwiftStdlib 5.9, *) {
      tests.test("assumeOnMainActorExecutor: wrongly assume the main executor, from DispatchQueue") {
        expectCrashLater(withMessage: "Expected same executor as actor 'Swift.MainActor' ('MainActorExecutor'), but was executing on '<unknown>'.")
        await withCheckedContinuation { cc in
          q.async {
            preconditionTaskOnActorExecutor(MainActor.shared)
            cc.resume()
          }
        }
      }
    }

    await runAllTestsAsync() // run synchronously
  }
}
