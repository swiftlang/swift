// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

// Mixed Int/Double slow to emit diagnostics
func rdar33476240(col: Int, row: Int, maxCol: Int, maxRow: Int) {
  let _ = (-(maxCol - 1) + (col * 2)) * 0.1
  // expected-error@-1 {{binary operator '*' cannot be applied to operands of type 'Int' and 'Double'}}
  // expected-note@-2 {{overloads for '*' exist with these partially matching parameter lists: (Double, Double), (Int, Int)}}
}
