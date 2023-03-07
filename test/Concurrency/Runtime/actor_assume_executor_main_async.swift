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

//import StdlibUnittest

func checkAssumeMainActor() /* synchronous! */ {
  assumeOnMainActorExecutor {
    print("OK")
  }
}

/// Crash tests, in order to make sure we report the expected diagnostics.
@main struct Main {
  static func main() async {
    if #available(SwiftStdlib 5.9, *) {
      // running directly without the Unittest suite as that would cause a hop off the main actor
      checkAssumeMainActor()
    }
  }
}
