// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

// Mixed Float and Double arithmetic
func rdar18800950(v: Float) -> Double {
  let c1: Float = 1.0
  let c2 = 2.0
  let r = v / c1
  let _ = (c2 * 2 * (3 * (1 - c1 / v) - 4) * r + 5) * (c2 * 2 * (3 * (1 - c1 / v) - 4) * r + 5)
  // expected-error@-1 {{reasonable time}}
}
