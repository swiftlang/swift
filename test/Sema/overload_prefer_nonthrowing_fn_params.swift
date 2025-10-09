// RUN: %target-typecheck-verify-swift -disable-availability-checking
// REQUIRES: concurrency

import _Concurrency

// Two overloads differing by throws on a function-typed parameter in a
// multi-parameter signature. The solver should prefer the non-throwing one
// when we pass a non-throwing closure literal.

func use(_ a: Int, _ op: () async -> Void) {}
func use(_ a: Int, _ op: () async throws -> Void) {}

func t1() {
  use(1) { } // expected-no-diagnostics (prefers non-throwing)
}

func mayThrow() async throws {}

func t2() {
  use(1) { try await mayThrow() } // expected-no-diagnostics (throwing ok)
}

// Mismatch at a different index (e.g., first param is function-typed)
func call(_ op: () async -> Void, _ b: Int) {}
func call(_ op: () async throws -> Void, _ b: Int) {}

func t3() {
  call({ }, 5) // expected-no-diagnostics (prefers non-throwing)
}
