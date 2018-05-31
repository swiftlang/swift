// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// TensorFlow Raw Ops API tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var RawOpsTests = TestSuite("RawOps")

RawOpsTests.testAllBackends("ArithmeticOps") {
  let lhsScalars: [Float] = [3, 1, 4, 1, 5, 9, 2, 6]
  let rhsScalars: [Float] = [2, 7, 1, 8, 2, 8, 1, 8]
  let shape = [2, 4]
  let tensorShape: TensorShape = TensorShape(shape.map { Int32($0) })
  let lhs = Tensor<Float>(shape: tensorShape, scalars: lhsScalars)
  let rhs = Tensor<Float>(shape: tensorShape, scalars: rhsScalars)

  let add = Raw.add(x: lhs, y: rhs)
  let sub = Raw.sub(x: lhs, y: rhs)
  let mul = Raw.mul(x: lhs, y: rhs)
  let div = Raw.div(x: lhs, y: rhs)

  expectEqual(ShapedArray(shape: shape,
                          scalars: zip(lhsScalars, rhsScalars).map { $0 + $1 }),
              add.array)

  expectEqual(ShapedArray(shape: shape,
                          scalars: zip(lhsScalars, rhsScalars).map { $0 - $1 }),
              sub.array)

  expectEqual(ShapedArray(shape: shape,
                          scalars: zip(lhsScalars, rhsScalars).map { $0 * $1 }),
              mul.array)

  expectEqual(ShapedArray(shape: shape,
                          scalars: zip(lhsScalars, rhsScalars).map { $0 / $1 }),
              div.array)
}

#if CPU && !CUDA
runAllTestsWithRemoteSession()
#else
runAllTests()
#endif // CPU && !CUDA
