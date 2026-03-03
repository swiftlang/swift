// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000
// REQUIRES: tools-release,no_asan

// https://github.com/apple/swift/issues/62390
// Compiler is extremely slow to compile simple closure

func testFn<U>(_: ((Int, Int)) -> U) {}

testFn { (a, _) in
  return ((a <= a && a >= a) || (a <= a && a >= a)) // Ok
}
