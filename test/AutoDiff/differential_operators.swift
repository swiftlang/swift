// RUN: %target-run-simple-parse-stdlib-swift
// REQUIRES: executable_test

import Swift
import StdlibUnittest

var BuiltinDifferentialOperatorTests = TestSuite("BuiltinDifferentialOperators")

@inlinable
func valueWithPullback<T, R>(
  at x: T, in f: @autodiff (T) -> R
) -> (R, (R.CotangentVector) -> T.CotangentVector)
  where T : Differentiable, R : Differentiable {
  return Builtin.autodiffGetVJP(f)(x)
}

// FIXME(rxwei): It's crashing because the compiler does not know how to emit reabstraction
// thunks for @autodiff functions yet.
// BuiltinDifferentialOperatorTests.test("Trivial") {
//   let t = 1.0
//   let (y, pullback) = valueWithPullback(at: 4.0) { x in
//     x * x * t
//   }
//   expectEqual(8, pullback(1))
// }

runAllTests()

