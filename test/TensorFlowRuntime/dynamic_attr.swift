// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// This file contains testing over dynamic attributes.

import CTensorFlow
import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicAttributeTests = TestSuite("DynamicAttribute")

// TODO: support this test in eager mode.
var val: Bool = true
_RuntimeConfig.usesTFEagerAPI = true // TODO: turn on by default
DynamicAttributeTests.testCPUOrGPU("DynAttr") {
  @inline(never)
  func dynAttrTest(x: Tensor<Float>, y: Tensor<Float>) {
    _RuntimeConfig.usesTFEagerAPI = true // TODO: turn on by default
    _RuntimeConfig.printsDebugLog = true
    // TODO: fix silgen crash when we replace `b` and use `val` directly.
    let b = val
    let z: TensorHandle<Float> = #tfop("MatMul", x, y, transpose_a: b)
    _hostOp(z)
    let z_tensor = Tensor(handle: z)
    expectEqual([1,1], z_tensor.shape)
    expectPointwiseNearlyEqual([2.0], z_tensor.scalars)
  }
  dynAttrTest(x: Tensor([[1]]), y: Tensor([[2]]))
}

runAllTests()
