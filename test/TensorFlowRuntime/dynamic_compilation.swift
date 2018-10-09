// RUN: %target-run-simple-swift
// TODO: Revert to %target-run-simple-swift once we fold dynamic compilation into -Onone.
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// This file contains testing over a dataset as a global variable. This requires
// sends/recvs support for variant handles.

import CTensorFlow
import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicCompilationTests = TestSuite("DynamicCompilation")

DynamicCompilationTests.testCPUOrGPU("Const") {
  _RuntimeConfig.printsDebugLog = true
  let x: TensorHandle<Float> = #tfop("Const", dtype$dtype: Float.tensorFlowDataType, value$tensor: Float(1.0))
  _hostOp(x)
  expectNearlyEqualWithScalarTensor(1.0, Tensor<Float>(handle: x))
}

DynamicCompilationTests.testCPUOrGPU("ScalarNonConst") {
  _RuntimeConfig.printsDebugLog = true
  func scalarInitializer_CreateHostTensor(_ x: Float) {
    let y = Tensor<Float>(x)
    _hostOp(y)
    expectNearlyEqualWithScalarTensor(x, y)
  }
  scalarInitializer_CreateHostTensor(1.2)
}

DynamicCompilationTests.testCPUOrGPU("AddFloat") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Float>(1.0)
  let y = Tensor<Float>(2.0)
  let z = x + y
  _hostOp(z)
  expectNearlyEqualWithScalarTensor(3.0, z)
}

DynamicCompilationTests.testCPUOrGPU("AddInt64") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Int64>(1)
  let y = Tensor<Int64>(2)
  let z = x + y
  _hostOp(z)
  expectEqualWithScalarTensor(3, z)
}

DynamicCompilationTests.testCPUOrGPU("AddInt32") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Int32>(1)
  let y = Tensor<Int32>(2)
  let z = x + y
  _hostOp(z)
  expectEqualWithScalarTensor(3, z)
}

DynamicCompilationTests.testCPUOrGPU("2Adds") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Float>(1.0)
  let y = Tensor<Float>(2.0)
  let z = x + y
  let w = z + z
  _hostOp(w)
  expectNearlyEqualWithScalarTensor(6.0, w)
}

runAllTests()
