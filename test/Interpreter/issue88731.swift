// RUN: %empty-directory(%t)
// RUN: %target-build-swift -language-mode 6 -target %target-swift-5.1-abi-triple %s -parse-as-library -module-name main -o %t/main
// RUN: %target-codesign %t/main
// RUN: %target-run %t/main | %FileCheck %s

// REQUIRES: concurrency
// REQUIRES: concurrency_runtime
// REQUIRES: executable_test

// UNSUPPORTED: back_deployment_runtime

// https://github.com/swiftlang/swift/issues/88731

@MainActor
func compute(_ fn: nonisolated(nonsending) @Sendable () async throws -> Void) async throws {
  try await fn()
  assertIsMainActor()
}

func assertIsMainActor() {
    MainActor.assumeIsolated {}
}

func performComputation() async throws {
  let x: Int = 42
  try await compute { [x] in
    print(x)
  }
}

@main struct Main {
  static func main() async throws {
    try await performComputation()
    // CHECK: 42
  }
}
