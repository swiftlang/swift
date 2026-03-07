// RUN: %target-typecheck-verify-swift -solver-scope-threshold=100
// REQUIRES: tools-release,no_asan

// This regressed since 6.3

func f(_: Int32) {}
func f(_: String) {}

func test() {
  let _ = f((0 * 0) + (0 * 0) + (0 * 0) + (0 * 0))
  // expected-error@-1 {{reasonable time}}
}

