// RUN: %target-run-simple-swift
//
// TODO: Disabling deabstration-inlining during dynamic compilation broke this test.
// : %target-run-dynamic-compilation-swift
//
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
// REQUIRES: tensorflow_swift_bindings
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

@_inlineable @inline(__always)
public func testPointwiseBinaryOp<T : AccelerableByTensorFlow & Equatable>(
  tfOp: (Tensor<Float>, Tensor<Float>) -> Tensor<T>,
  swiftOp: (Float, Float) -> T) {
  let lhsScalars: [Float] = [3, 1, 4, 1, 5, 9, 2, 7]
  let rhsScalars: [Float] = [2, 7, 1, 8, 2, 8, 1, 7]
  let shape = [2, 4]
  let tensorShape: TensorShape = TensorShape(shape.map { Int32($0) })
  let lhs = Tensor<Float>(shape: tensorShape, scalars: lhsScalars)
  let rhs = Tensor<Float>(shape: tensorShape, scalars: rhsScalars)

  let tfResult = tfOp(lhs, rhs)
  expectEqual(ShapedArray(shape: shape,
                          scalars: zip(lhsScalars, rhsScalars).map(swiftOp)),
              tfResult.array)
}

// TODO(mazare): group all these tests in a single function once this does
// not cause an XLA compilation error anymore.
RawOpsTests.testAllBackends("AddOp") {
  testPointwiseBinaryOp(tfOp: Raw.add, swiftOp: { $0 + $1 })
}

RawOpsTests.testAllBackends("SubOp") {
  testPointwiseBinaryOp(tfOp: Raw.sub, swiftOp: { $0 - $1 })
}

RawOpsTests.testAllBackends("MulOp") {
  testPointwiseBinaryOp(tfOp: Raw.mul, swiftOp: { $0 * $1 })
}

RawOpsTests.testAllBackends("DivOp") {
  testPointwiseBinaryOp(tfOp: Raw.div, swiftOp: { $0 / $1 })
}

RawOpsTests.testAllBackends("FloorDivOp") {
  testPointwiseBinaryOp(tfOp: Raw.floorDiv, swiftOp: { ($0 / $1).rounded(.down) })
}

RawOpsTests.testAllBackends("MinimumOp") {
  testPointwiseBinaryOp(tfOp: Raw.minimum, swiftOp: min)
}

RawOpsTests.testAllBackends("MaximumOp") {
  testPointwiseBinaryOp(tfOp: Raw.maximum, swiftOp: max)
}

RawOpsTests.testAllBackends("MaximumOp") {
  testPointwiseBinaryOp(tfOp: Raw.maximum, swiftOp: max)
}

RawOpsTests.testAllBackends("EqualOp") {
  testPointwiseBinaryOp(tfOp: Raw.equal, swiftOp: { $0 == $1 })
}

RawOpsTests.testAllBackends("LessOp") {
  testPointwiseBinaryOp(tfOp: Raw.less, swiftOp: { $0 < $1 })
}

RawOpsTests.testAllBackends("LessEqualOp") {
  testPointwiseBinaryOp(tfOp: Raw.lessEqual, swiftOp: { $0 <= $1 })
}

RawOpsTests.testAllBackends("GreaterOp") {
  testPointwiseBinaryOp(tfOp: Raw.greater, swiftOp: { $0 > $1 })
}

RawOpsTests.testAllBackends("GreaterEqualOp") {
  testPointwiseBinaryOp(tfOp: Raw.greaterEqual, swiftOp: { $0 >= $1 })
}

runAllTests()
