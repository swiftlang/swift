// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SupersetAdjointTests = TestSuite("SupersetAdjoint")

@differentiable(reverse, wrt: (.0, .1), adjoint: dmulxy)
func mulxy(_ x: Float, _ y: Float) -> Float {
  // use control flow to prevent AD; NB fix when control flow is supported
  if x > 1000 {
    return y
  }
  return x * y
}
func dmulxy(_ x: Float, _ y: Float, primal: Float, seed: Float)
  -> (Float, Float) {
  return (y * seed, x * seed)
}

func calls_mulxy(_ x: Float, _ y: Float) -> Float {
  return mulxy(x, y)
}

SupersetAdjointTests.test("Superset") {
  expectEqual(3, #gradient(mulxy, wrt: .0)(2, 3))
}

SupersetAdjointTests.test("SupersetNested") {
  expectEqual(2, #gradient(calls_mulxy, wrt: .1)(2, 3))
}

SupersetAdjointTests.test("CrossModuleClosure") {
  expectEqual(1, #gradient({ (x: Float, y: Float) in x + y }, wrt: .0)(1, 2))
}

// TODO: unbreak this test
// SupersetAdjointTests.test("CrossModule") {
//   expectEqual(1, #gradient((+) as (Float, Float) -> Float, wrt: .0)(1, 2))
// }

// TODO: unbreak this one too
// @differentiable(reverse, wrt: (.0, .1), adjoint: dx_T)
// func x_T<T>(_ x: Float, _ y: T) -> Float {
//   if x > 1000 {
//     return x
//   }
//   return x
// }
// func dx_T<T>(
//     _ x: Float, _ y: T, primal: Float, seed: Float) -> (Float, T) {
//   return (x, y)
// }
// SupersetAdjointTests.test("IndirectResults") {
//   expectEqual(3, #gradient(x_T, wrt: .0)(2, Float(3)))
// }

runAllTests()
