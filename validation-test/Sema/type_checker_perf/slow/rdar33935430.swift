// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression: Missing (Int, Float) overload of +

func rdar33935430(a: Int, b: Int, c: Float, d: Float, n: Int) {
  let _ = ((a * c) + ((b * d - a * c) / (b - a)) * (n - a)) / n
  // expected-error@-1 {{reasonable time}}
}
