// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000 -solver-enable-performance-hacks
// RUN: %target-typecheck-verify-swift -solver-scope-threshold=15000 -solver-disable-performance-hacks
// REQUIRES: tools-release,no_asan

// UNSUPPORTED: OS=linux-gnu

// rdar://170773776
// XFAIL: OS=freebsd

let a: [Double] = []
_ = a.map { $0 - 1.0 }
     .map { $0 * $0 }
     .reduce(0, +) / Double(a.count)
