// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

func test() {
  let x: UInt = 1 * 2 + 3 * 4 + 5 * 6 + 7 * 8 + 9 * 10 + 11 * 12 + 13 * 14
  // expected-error@-1 {{reasonable time}}
}

