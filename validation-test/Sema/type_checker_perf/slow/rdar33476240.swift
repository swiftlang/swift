// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression: Missing (Int, Double) overload of - or *

func rdar33476240(col: Int, row: Int, maxCol: Int, maxRow: Int) {
  let _ = (-(maxCol - 1) + (col * 2)) * 0.1 * 0.2 * 0.3
  // expected-error@-1 {{reasonable time}}
}
