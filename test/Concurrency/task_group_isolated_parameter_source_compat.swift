// RUN: %target-swift-frontend %s -emit-sil -o /dev/null -verify -strict-concurrency=complete

// REQUIRES: concurrency

// Verify source compatibility: code using explicit `isolation:` parameter compiles
// and produces deprecation warnings, guiding users toward nonisolated(nonsending) overloads.

@available(SwiftStdlib 6.2, *)
actor MyActor {
  func test() async throws {
    await withTaskGroup(of: Int.self, isolation: self) { group in // expected-warning{{deprecated}}
      group.addTask { 1 }
    }

    await withThrowingTaskGroup(of: Int.self, isolation: self) { group in // expected-warning{{deprecated}}
      group.addTask { 1 }
    }

    await withDiscardingTaskGroup(isolation: self) { group in // expected-warning{{deprecated}}
      group.addTask { }
    }

    try await withThrowingDiscardingTaskGroup(isolation: self) { group in // expected-warning{{deprecated}}
      group.addTask { }
    }
  }
}
