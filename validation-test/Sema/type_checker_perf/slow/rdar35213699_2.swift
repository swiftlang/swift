// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000
// REQUIRES: tools-release,no_asan

func test() {
  let _ = UInt((0 * 0) + (0 * 0) + (0 * 0) + (0 * 0))
  // expected-error@-1 {{reasonable time}}
}

