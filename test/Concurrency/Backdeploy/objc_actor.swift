// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target x86_64-apple-macosx10.15 %s -o %t/test_backdeploy -Xfrontend -parse-as-library
// RUN: %target-run %t/test_backdeploy

// REQUIRES: CPU=x86_64
// REQUIRES: OS=macosx
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime

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
