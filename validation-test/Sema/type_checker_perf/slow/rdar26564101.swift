// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts
// REQUIRES: rdar46850561

func rdar26564101(a: [Double], m: Double) -> Double {
  return Double(Array(0...a.count - 1).reduce(0) { $0 + $1 - m })
  // expected-error@-1 {{too complex}}
}
