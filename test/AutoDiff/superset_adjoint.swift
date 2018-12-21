// RUN: %target-run-simple-swift
// RUN: %target-run-simple-no-vjp-swift
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
func dmulxy(_ seed: Float, _ primal: Float, _ x: Float, _ y: Float)
  -> (Float, Float) {
  return (y * seed, x * seed)
}

func calls_mulxy(_ x: Float, _ y: Float) -> Float {
  return mulxy(x, y)
}

SupersetAdjointTests.test("Superset") {
  expectEqual(3, gradient(at: 2) { x in mulxy(x, 3) })
}

SupersetAdjointTests.test("SupersetNested") {
  expectEqual(2, gradient(at: 3) { y in calls_mulxy(2, y) })
}

SupersetAdjointTests.test("CrossModuleClosure") {
  expectEqual(1, gradient(at: Float(1)) { x in x + 2 })
}

SupersetAdjointTests.test("CrossModule") {
  expectEqual((1, 1), gradient(at: 1, 2, in: (+) as (Float, Float) -> Float))
}

// FIXME: Unbreak this one.
//
// @differentiable(reverse, wrt: (.0, .1), vjp: dx_T)
// func x_T<T : Differentiable>(_ x: Float, _ y: T) -> Float {
//   if x > 1000 {
//     return x
//   }
//   return x
// }
// func dx_T<T>(_ x: Float, _ y: T) -> (Float, (Float) -> (Float, T.TangentVector)) {
//   return (x_T(x, y), { seed in (x, y) })
// }
// SupersetAdjointTests.test("IndirectResults") {
//   expectEqual(3, gradient(at: 2) { x in x_T(x, Float(3)) })
// }

runAllTests()
