// RUN: %target-typecheck-verify-swift -solver-scope-threshold=50000
// REQUIRES: tools-release,no_asan
// UNSUPPORTED: swift_test_mode_optimize_none && OS=linux-gnu

// Invalid expression: Missing (Int, Double) overload of -

func rdar26564101(a: [Double], m: Double) -> Double {
  // expected-error@+1 {{unable to type-check this expression in reasonable time}}
  return Double(Array(0...a.count - 1).reduce(0) {
    $0 + $1 - m + $0 + $1 + $0
  })
}
