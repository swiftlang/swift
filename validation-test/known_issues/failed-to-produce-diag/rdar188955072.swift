// rdar://188955072
// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

func test() async throws {
  try await withCheckedThrowingContinuation { (cont: CheckedContinuation<(), any Error>) in
  }
  try await withCheckedThrowingContinuation { cont in
    // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
  }
}
