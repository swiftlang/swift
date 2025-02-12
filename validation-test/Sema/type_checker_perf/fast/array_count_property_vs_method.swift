// RUN: %target-typecheck-verify-swift -solver-scope-threshold=11000
// REQUIRES: tools-release,no_asan
// REQUIRES: OS=macosx

func f(n: Int, a: [Int]) {
  let _ = [(0 ..< n + a.count).map { Int8($0) }] +
          [(0 ..< n + a.count).map { Int8($0) }.reversed()] // Ok
}
