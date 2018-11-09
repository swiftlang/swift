// RUN: %target-run-simple-swift
// RUN: %target-run-dynamic-compilation-swift
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
public func testPointwiseBinaryOp<T : TensorFlowScalar & Equatable>(
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
// TODO(marcrasi): Pass `Raw.xxxx` directly to `testPointwiseBinaryOp` rather
// than wrapping it in a closure, once IRGen can handle non-deabstracted
// functions.
RawOpsTests.testAllBackends("AddOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.add(x, y) }, swiftOp: { $0 + $1 })
}

RawOpsTests.testAllBackends("SubOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.sub(x, y) }, swiftOp: { $0 - $1 })
}

RawOpsTests.testAllBackends("MulOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.mul(x, y) }, swiftOp: { $0 * $1 })
}

RawOpsTests.testAllBackends("DivOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.div(x, y) }, swiftOp: { $0 / $1 })
}

RawOpsTests.testAllBackends("FloorDivOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.floorDiv(x, y) }, swiftOp: { ($0 / $1).rounded(.down) })
}

RawOpsTests.testAllBackends("MinimumOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.minimum(x, y) }, swiftOp: min)
}

RawOpsTests.testAllBackends("MaximumOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.maximum(x, y) }, swiftOp: max)
}

RawOpsTests.testAllBackends("MaximumOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.maximum(x, y) }, swiftOp: max)
}

RawOpsTests.testAllBackends("EqualOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.equal(x, y) }, swiftOp: { $0 == $1 })
}

RawOpsTests.testAllBackends("LessOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.less(x, y) }, swiftOp: { $0 < $1 })
}

RawOpsTests.testAllBackends("LessEqualOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.lessEqual(x, y) }, swiftOp: { $0 <= $1 })
}

RawOpsTests.testAllBackends("GreaterOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.greater(x, y) }, swiftOp: { $0 > $1 })
}

RawOpsTests.testAllBackends("GreaterEqualOp") {
  testPointwiseBinaryOp(tfOp: { (x: Tensor<Float>, y: Tensor<Float>) in Raw.greaterEqual(x, y) }, swiftOp: { $0 >= $1 })
}

runAllTests()
