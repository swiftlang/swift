// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest

var SupersetVJPTests = TestSuite("SupersetVJP")

@differentiable(wrt: (.0, .1), vjp: dmulxy)
func mulxy(_ x: Float, _ y: Float) -> Float {
  // use control flow to prevent AD; NB fix when control flow is supported
  if x > 1000 {
    return y
  }
  return x * y
}
func dmulxy(_ x: Float, _ y: Float) -> (Float, (Float) -> (Float, Float)) {
  return (mulxy(x, y), { v in (y * v, x * v) })
}

func calls_mulxy(_ x: Float, _ y: Float) -> Float {
  return mulxy(x, y)
}

SupersetVJPTests.test("Superset") {
  expectEqual(3, gradient(at: 2) { x in mulxy(x, 3) })
}

SupersetVJPTests.test("SupersetNested") {
  expectEqual(2, gradient(at: 3) { y in calls_mulxy(2, y) })
}

SupersetVJPTests.test("CrossModuleClosure") {
  expectEqual(1, gradient(at: Float(1)) { x in x + 2 })
}

// FIXME: The expression `(+) as @autodiff (Float, @nondiff Float) -> Float)`
// forms a curry thunk of `Float.+` before conversion to @autodiff, and AD
// doesn't know how to differentiate the curry thunk, so it produces a
// "function is not differentiable" error.
// FIXME: Propagate wrt indices correctly so that this actually takes the
// gradient wrt only the first parameter, as intended.
// SupersetVJPTests.test("CrossModule") {
//   expectEqual(1, gradient(at: 1, 2, in: (+) as @autodiff (Float, @nondiff Float) -> Float))
// }

// FIXME: Unbreak this one.
//
// @differentiable(wrt: (.0, .1), vjp: dx_T)
// func x_T<T : Differentiable>(_ x: Float, _ y: T) -> Float {
//   if x > 1000 {
//     return x
//   }
//   return x
// }
// func dx_T<T>(_ x: Float, _ y: T) -> (Float, (Float) -> (Float, T.TangentVector)) {
//   return (x_T(x, y), { seed in (x, y) })
// }
// SupersetVJPTests.test("IndirectResults") {
//   expectEqual(3, gradient(at: 2) { x in x_T(x, Float(3)) })
// }

runAllTests()
