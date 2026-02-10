// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

func testNonisolatedNonsendingMetadata() {
  typealias NonisolatedNonsendingFn = nonisolated(nonsending) () async throws -> Int

  let nonisolatedType = NonisolatedNonsendingFn.self

  // CHECK: nonisolated(nonsending) type: {{.*}}() async throws -> Int
  print("nonisolated(nonsending) type: \(nonisolatedType)")
}

testNonisolatedNonsendingMetadata()
