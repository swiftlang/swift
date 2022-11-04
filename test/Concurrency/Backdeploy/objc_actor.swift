// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-cpu-apple-macosx11 %s -o %t/test_backdeploy -Xfrontend -parse-as-library
// RUN: %target-run %t/test_backdeploy

// REQUIRES: OS=macosx
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: objc_interop
// UNSUPPORTED: back_deployment_runtime

import Foundation

@objc actor MyActor {
  func f() -> String { "hello" }
}

@main
enum Main {
  static func main() async {
    let ma = MyActor()
    let greeting = await ma.f()
    assert(greeting == "hello")
  }
}
