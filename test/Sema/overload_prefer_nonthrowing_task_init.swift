// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency

import _Concurrency

// Minimal repro: prefer non-throwing Task.init when the closure literal is non-throwing.
func minimalRepro() {
  _ = Task { } // expected-no-diagnostics
}

// If the closure *does* throw, selection must still succeed (choosing the throwing init).
func mayThrow() async throws {}
func throwingStillResolves() {
  _ = Task { try await mayThrow() } // expected-no-diagnostics
}

// Add some generic/optional noise to approximate the original ambiguity conditions.
struct Box<T> { var value: T? }

func genericContext<T>(_ x: Box<T>) {
  _ = Task { } // expected-no-diagnostics
}
