// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import _Differentiation

var SubsetParameterThunkTests = TestSuite("SubsetParameterThunks")

func inoutDirect(_ x: Float, _ y: inout Double, _ z: Float) {}

@derivative(of: inoutDirect)
func vjpInoutDirect(_ x: Float, _ y: inout Double, _ z: Float) -> (
  value: Void, pullback: (inout Double) -> (Float, Float)
) {
  return ((), { dy in
    dy = 3
    return (2, 4)
  })
}

SubsetParameterThunkTests.test("InoutParametersDirect") {
  @differentiable(wrt: x)
  @differentiable(wrt: y)
  @differentiable(wrt: z)
  func inoutDirectCaller(_ x: Float, _ y: Double, _ z: Float) -> Double {
    var result = y
    inoutDirect(x, &result, z)
    return result
  }

  let x: Float = 3
  let y: Double = 4
  let z: Float = 5
  expectEqual((2, 3, 4), gradient(at: x, y, z, in: inoutDirectCaller))
  expectEqual((3, 4), gradient(at: y, z, in: { y, z in inoutDirectCaller(x, y, z) }))
  expectEqual((2, 4), gradient(at: x, z, in: { x, z in inoutDirectCaller(x, y, z) }))
  expectEqual((2, 3), gradient(at: x, y, in: { x, y in inoutDirectCaller(x, y, z) }))
}

func inoutIndirect<T: Differentiable, U: Differentiable, V: Differentiable>(
  _ x: T, _ y: inout U, _ z: V
) {}

@derivative(of: inoutIndirect)
func vjpInoutIndirect<T: Differentiable, U: Differentiable, V: Differentiable>(
  _ x: T, _ y: inout U, _ z: V
) -> (
  value: Void, pullback: (inout U.TangentVector) -> (T.TangentVector, V.TangentVector)
) {
  return ((), { dy in
    return (.zero, .zero)
  })
}

SubsetParameterThunkTests.test("InoutParametersIndirect") {
  @differentiable(wrt: x)
  @differentiable(wrt: y)
  @differentiable(wrt: z)
  @differentiable
  func inoutIndirectCaller<T: Differentiable, U: Differentiable, V: Differentiable>(
    _ x: T, _ y: U, _ z: V
  ) -> U {
    var result = y
    inoutIndirect(x, &result, z)
    return result
  }

  let x: Float = 3
  let y: Double = 4
  let z: Float = 5
  expectEqual((0, 1, 0), gradient(at: x, y, z, in: inoutIndirectCaller))
  expectEqual((1, 0), gradient(at: y, z, in: { y, z in inoutIndirectCaller(x, y, z) }))
  expectEqual((0, 0), gradient(at: x, z, in: { x, z in inoutIndirectCaller(x, y, z) }))
  expectEqual((0, 1), gradient(at: x, y, in: { x, y in inoutIndirectCaller(x, y, z) }))
}

runAllTests()
