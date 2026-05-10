// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan

// Invalid expression: Missing (Double, Float) overload of /

func fun(_ x: Double) -> Double { fatalError() }

func test(l: Double, s: Float) {
  _ = fun((l / 2.0) * (l / 2.0) * (l / 2.0) / (1.0 + s * s))
  // expected-error@-1 {{reasonable time}}
}
