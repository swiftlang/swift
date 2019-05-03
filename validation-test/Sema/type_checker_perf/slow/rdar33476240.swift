// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

// Mixed Int/Double slow to emit diagnostics
func rdar33476240(col: Int, row: Int, maxCol: Int, maxRow: Int) {
  let _ = (-(maxCol - 1) + (col * 2)) * 0.1 * 0.2 * 0.3
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
}
