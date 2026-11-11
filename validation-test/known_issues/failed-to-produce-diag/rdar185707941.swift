// rdar://185707941
// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

struct Worker: ~Copyable {
  consuming func work() async throws {
    try? withCheckedThrowingContinuation { continuation in
    // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
      Task {
        try await withThrowingTaskGroup { group in
          return self
        }
      }
    }
  }
}
