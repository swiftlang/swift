// RUN: %target-run-simple-swift | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: concurrency

func testNonisolatedNonsendingMetadata() {
  typealias NonisolatedNonsendingFn = nonisolated(nonsending) () async throws -> Int

  let nonisolatedType = NonisolatedNonsendingFn.self

  // We are just testing that we do not crash but we want to make sure we emit
  // output. Some notes:
  //
  // 1. Given a new enough runtime, we should print out:
  //
  //    nonisolated(nonsending) () async throws -> Int
  //
  // 2. Given a semi-old runtime (before Swift 6), we should print out:
  //
  //    () async throws -> Int
  //
  // 3. Given a very old runtime (before Swift 5.5), we should due to the usage
  // of the concurrency compatibility print out:
  //
  //    () throws -> Int

  if #available(macOS 15.0, iOS 18.0, watchOS 11.0, tvOS 18.0, visionOS 2.0, *) {
    print("Result: nonisolated(nonsending) () async throws -> Int")
  } else if #available(macOS 12.0, iOS 15.0, watchOS 8.0, tvOS 15.0, *) {
    print("Result: () async throws -> Int")
  } else {
    print("Result: () throws -> Int")
  }
  // CHECK: Result: [[RESULT:.*]]

  // CHECK: nonisolated(nonsending) type: [[RESULT]]
  print("nonisolated(nonsending) type: \(nonisolatedType)")
}

testNonisolatedNonsendingMetadata()
