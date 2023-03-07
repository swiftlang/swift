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

func checkAssumeMainActor() /* synchronous! */ {
  assumeOnMainActorExecutor {
    print("OK")
  }
}

@main struct Main {
  static func main() /* synchronous! */ {
    let tests = TestSuite("AssumeActorExecutor")

    if #available(SwiftStdlib 5.9, *) {
      // === MainActor --------------------------------------------------------

      tests.test("assumeOnMainActorExecutor: assume the main executor, from 'main()'") {
        preconditionTaskOnActorExecutor(MainActor.shared)
        checkAssumeMainActor()
      }

      tests.test("assumeOnMainActorExecutor: assume the main executor, from 'Task{}' inheriting MainActor isolation") {
        Task {
          // we properly inferred MainActor isolation
          preconditionTaskOnActorExecutor(MainActor.shared)
          checkAssumeMainActor()
        }
      }

      tests.test("assumeOnMainActorExecutor: assume the main executor, from 'Task{ @MainActor in }' isolation") {
        Task {
          preconditionTaskOnActorExecutor(MainActor.shared)
          checkAssumeMainActor()
        }
      }
    }

    runAllTests() // run synchronously
  }
}
