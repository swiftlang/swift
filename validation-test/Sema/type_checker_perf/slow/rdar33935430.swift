// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

// Mixed Int/Float arithmetic
func rdar33935430(a: Int, b: Int, c: Float, d: Float, n: Int) {
  let _ = ((a * c) + ((b * d - a * c) / (b - a)) * (n - a)) / n
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
}
