// rdar://184663984
// RUN: %target-typecheck-verify-swift

// REQUIRES: concurrency

func foo() async {
  let bar = await withCheckedContinuation { continuation in continuation.resume(with: 0) }
  // expected-error@-1 {{failed to produce diagnostic for expression; please submit a bug report}}
}
