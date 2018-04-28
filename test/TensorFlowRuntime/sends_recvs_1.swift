// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// Swift <-> TF sends/recvs tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var SendsRecvsTests = TestSuite("SendsRecvs")

// FIXME: Add GPU and TPU Support.
@inline(never)
func test1Send() {
#if !CUDA
  _RuntimeConfig.usesTFEagerAPI = false
  var a = Tensor<Float>(1.0)
  // One send.
  print(a.toHost())
  a += 1
  // This one should not be a send.
  print(a.toHost())
  expectEqual(2, a.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("test1Send", test1Send)

@inline(never)
func test2Sends() {
#if !CUDA
  _RuntimeConfig.usesTFEagerAPI = false
  var a = Tensor<Float>(1.0)
  // One send.
  print(a.toHost())
  a += 2
  // Another send.
  print(a.toHost())
  a += 3
  // This one should not be a send.
  print(a.toHost())
  expectEqual(6, a.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("test2Sends", test2Sends)

@inline(never)
func testSendsInALoop() {
#if !CUDA
  _RuntimeConfig.usesTFEagerAPI = false
  let maxCount = 3
  var count = 0
  var a = Tensor<Float>(0.0)
  let b = Tensor<Float>(1.0)
  while count < maxCount {
    a += b
    // One send.
    print(a.toHost())
    count += 1
  }
  a += a
  // This is not a send.
  print("final a = \(a.toHost())")
  expectEqual(3 * 2, a.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("testSendsInALoop", testSendsInALoop)

runAllTests()
