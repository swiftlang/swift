// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asserts
// REQUIRES: disabled-rdar35803518

let a: [Double] = []
_ = a.map { $0 - 1.0 }
// expected-error@-1 {{expression was too complex to be solved in reasonable time; consider breaking up the expression into distinct sub-expressions}}
     .map { $0 * $0 }
     .reduce(0, +) / Double(a.count)
