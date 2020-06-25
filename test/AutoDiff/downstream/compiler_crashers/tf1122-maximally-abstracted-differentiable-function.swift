// RUN: %target-build-swift %s -o %t
// RUN: not --crash %target-run %t
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize_none

func id<T>(_ t: T) -> T { t }
func foo<X: Differentiable>(_ x: X) {
  let f: @differentiable (X) -> X = { $0 }
  let _ = id(f)
}
foo(Float(1))
