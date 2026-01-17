// RUN: %empty-directory(%t)
// RUN: %target-build-swift -target %target-swift-5.1-abi-triple %s -default-isolation MainActor -enable-upcoming-feature NonisolatedNonsendingByDefault -parse-as-library -module-name main -swift-version 5 -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: executable_test
// REQUIRES: concurrency_runtime
// REQUIRES: swift_feature_NonisolatedNonsendingByDefault

// UNSUPPORTED: back_deployment_runtime
// UNSUPPORTED: OS=windows-msvc

struct S {
  var action: (Int) async -> Void

  init(action: @escaping (Int) -> Void) {
    self.action = action
  }
}

@main
enum App {
  static func main() async {
    let s = S { print($0) }
    await s.action(42)
    // CHECK: 42
  }
}
