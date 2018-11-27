// RUN: %target-run-simple-swift  | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
#if os(macOS) || os(iOS) || os(watchOS) || os(tvOS)
import Darwin
#else
import Glibc
#endif

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest
import CTensorFlow

var RemoteSessionTests = TestSuite("RemoteSessionTest")

// Just a dummy test to trigger Tensor-related code paths in compiler.
public func add(a: Tensor<Float>, b: Tensor<Float>) -> Tensor<Float> {
  return a + b
}

RemoteSessionTests.testAllBackends("dummyAddTest") {
  _RuntimeConfig.printsDebugLog  = true
  expectNearlyEqualWithScalarTensor(3.0, add(a: Tensor<Float>(1.0), b: Tensor<Float>(2.0)))
}

// CHECK-LABEL: There are 4 devices.
// CHECK: Device 0 has type CPU and name /job:localhost/replica:0/task:0/device:CPU:0.
// CHECK: Device 1 has type XLA_CPU and name /job:localhost/replica:0/task:0/device:XLA_CPU:0.
// CHECK: Device 2 has type CPU and name /job:localhost/replica:0/task:1/device:CPU:0.
// CHECK: Device 3 has type XLA_CPU and name /job:localhost/replica:0/task:1/device:XLA_CPU:0.

runAllTestsWithRemoteSession()
