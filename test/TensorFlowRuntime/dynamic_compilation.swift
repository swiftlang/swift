// RUN: %target-run-simple-swift %swift-tensorflow-test-run-extra-options
// REQUIRES: executable_test

import CTensorFlow
import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var DynamicCompilationTests = TestSuite("DynamicCompilation")

DynamicCompilationTests.testAllBackends("ScalarNonConst") {
  _RuntimeConfig.printsDebugLog = true
  func scalarInitializer_CreateHostTensor(_ x: Float) {
    let y = Tensor<Float>(x)
    expectNearlyEqualWithScalarTensor(x, y)
  }
  scalarInitializer_CreateHostTensor(1.2)
}

DynamicCompilationTests.testAllBackends("AddFloat") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Float>(1.0)
  let y = Tensor<Float>(2.0)
  let z = x + y
  expectNearlyEqualWithScalarTensor(3.0, z)
}

DynamicCompilationTests.testAllBackends("AddInt64") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Int64>(1)
  let y = Tensor<Int64>(2)
  let z = x + y
  expectEqualWithScalarTensor(3, z)
}

DynamicCompilationTests.testAllBackends("AddInt32") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Int32>(1)
  let y = Tensor<Int32>(2)
  let z = x + y
  expectEqualWithScalarTensor(3, z)
}

DynamicCompilationTests.testAllBackends("2Adds") {
  _RuntimeConfig.printsDebugLog = true
  let x = Tensor<Float>(1.0)
  let y = Tensor<Float>(2.0)
  let z = x + y
  let w = z + z
  expectNearlyEqualWithScalarTensor(6.0, w)
}

runAllTests()
