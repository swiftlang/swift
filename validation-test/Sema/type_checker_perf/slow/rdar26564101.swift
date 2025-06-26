// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1 -disable-constraint-solver-performance-hacks
// REQUIRES: tools-release,no_asan
// UNSUPPORTED: swift_test_mode_optimize_none && OS=linux-gnu

func rdar26564101(a: [Double], m: Double) -> Double {
  // expected-error@+1 {{unable to type-check this expression in reasonable time}}
  return Double(Array(0...a.count - 1).reduce(0) {
    $0 + $1 - m + $0 + $1 + $0
  })
}
