// RUN: %target-run-simple-swift(-Xfrontend -requirement-machine=off)
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
  @differentiable(reverse, wrt: x)
  @differentiable(reverse, wrt: y)
  @differentiable(reverse, wrt: z)
  func inoutDirectCaller(_ x: Float, _ y: Double, _ z: Float) -> Double {
    var result = y
    inoutDirect(x, &result, z)
    return result
  }

  let x: Float = 3
  let y: Double = 4
  let z: Float = 5
  expectEqual((2, 3, 4), gradient(at: x, y, z, of: inoutDirectCaller))
  expectEqual((3, 4), gradient(at: y, z, of: { y, z in inoutDirectCaller(x, y, z) }))
  expectEqual((2, 4), gradient(at: x, z, of: { x, z in inoutDirectCaller(x, y, z) }))
  expectEqual((2, 3), gradient(at: x, y, of: { x, y in inoutDirectCaller(x, y, z) }))
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
  @differentiable(reverse, wrt: x)
  @differentiable(reverse, wrt: y)
  @differentiable(reverse, wrt: z)
  @differentiable(reverse)
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
  expectEqual((0, 1, 0), gradient(at: x, y, z, of: inoutIndirectCaller))
  expectEqual((1, 0), gradient(at: y, z, of: { y, z in inoutIndirectCaller(x, y, z) }))
  expectEqual((0, 0), gradient(at: x, z, of: { x, z in inoutIndirectCaller(x, y, z) }))
  expectEqual((0, 1), gradient(at: x, y, of: { x, y in inoutIndirectCaller(x, y, z) }))
}

runAllTests()
