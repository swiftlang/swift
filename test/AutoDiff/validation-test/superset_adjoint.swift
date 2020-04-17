// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var SupersetVJPTests = TestSuite("SupersetVJP")

@differentiable(wrt: (x, y))
func mulxy(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
  // use control flow to prevent AD; NB fix when control flow is supported
  if x > 1000 {
    return y
  }
  return x * y
}
@derivative(of: mulxy)
func dmulxy(
  _ x: Tracked<Float>,
  _ y: Tracked<Float>
) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Tracked<Float>, Tracked<Float>)) {
  return (mulxy(x, y), { v in (y * v, x * v) })
}

func calls_mulxy(_ x: Tracked<Float>, _ y: Tracked<Float>) -> Tracked<Float> {
  return mulxy(x, y)
}

SupersetVJPTests.testWithLeakChecking("Superset") {
  expectEqual(3, gradient(at: 2) { x in mulxy(x, 3) })
}

SupersetVJPTests.testWithLeakChecking("SupersetNested") {
  expectEqual(2, gradient(at: 3) { y in calls_mulxy(2, y) })
}

SupersetVJPTests.testWithLeakChecking("CrossModuleClosure") {
  expectEqual(1, gradient(at: Tracked<Float>(1)) { x in x + 2 })
}

SupersetVJPTests.testWithLeakChecking("SubsetOfSubset") {
  @differentiable(wrt: (x, z))
  func foo(_ x: Tracked<Float>, _ y: Tracked<Float>, _ z: Tracked<Float>) -> Tracked<Float> {
    withoutDerivative(at: 0)
  }
  expectEqual(0, gradient(at: 0, in: { x in foo(x, 0, 0) }))
}

SupersetVJPTests.test("ApplySubset") {
  // TF-914
  @differentiable(wrt: x)
  func foo<T: Differentiable>(_ x: T, _ y: T, apply: @differentiable (T, T) -> T) -> T {
    return apply(x, y)
  }
  expectEqual(1, gradient(at: Tracked<Float>(0)) { x in foo(x, 0) { $0 + $1 } })
}

// FIXME: The expression `(+) as @differentiable (Float, @noDerivative Float) -> Float)`
// forms a curry thunk of `Float.+` before conversion to @differentiable, and AD
// doesn't know how to differentiate the curry thunk, so it produces a
// "function is not differentiable" error.
// SupersetVJPTests.test("CrossModule") {
//   let grad = gradient(at: Float(1), Float(2), in: (+) as @differentiable (Float, @noDerivative Float) -> Float)
//   expectEqual(Float(1), grad)
// }

@differentiable(wrt: (x, y))
func x_T<T : Differentiable>(_ x: Tracked<Float>, _ y: T) -> Tracked<Float> {
  if x > 1000 { return x }
  return x
}
@derivative(of: x_T)
func dx_T<T : Differentiable>(
  _ x: Tracked<Float>, _ y: T
) -> (value: Tracked<Float>, pullback: (Tracked<Float>) -> (Tracked<Float>, T.TangentVector)) {
  return (x_T(x, y), { v in (x * v, .zero) })
}
SupersetVJPTests.testWithLeakChecking("IndirectResults") {
  expectEqual(2, gradient(at: 2) { x in x_T(x, Tracked<Float>(3)) })
}

runAllTests()
