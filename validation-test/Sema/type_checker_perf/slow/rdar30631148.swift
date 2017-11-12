// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts

func fun(_ x: Double) -> Double { fatalError() }

func test(l: Double, s: Float) {
  _ = fun((l / 2.0) * (l / 2.0) * (l / 2.0) / (1.0 + s * s))
  // expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
}
