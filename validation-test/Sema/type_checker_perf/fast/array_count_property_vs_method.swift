// RUN: %target-typecheck-verify-swift -solver-expression-time-threshold=5
// REQUIRES: tools-release,no_asan

func f(n: Int, a: [Int]) {
  let _ = [(0 ..< n + a.count).map { Int8($0) }] +
          [(0 ..< n + a.count).map { Int8($0) }.reversed()] // Ok
}
