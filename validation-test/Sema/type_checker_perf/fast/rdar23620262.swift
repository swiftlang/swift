// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=1
// REQUIRES: tools-release,no_asan

// UNSUPPORTED: OS=linux-gnu

// expected-no-diagnostics

// REQUIRES: 33958047

let a: [Double] = []
_ = a.map { $0 - 1.0 }
     .map { $0 * $0 }
     .reduce(0, +) / Double(a.count)
