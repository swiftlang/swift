// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Swift <-> TF sends/recvs tests.

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var SendsRecvsTests = TestSuite("SendsRecvs")

// FIXME: Add GPU and TPU Support.
@inline(never)
func test1Send() {
#if !CUDA
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

func test1RecvFloatScalar() {
#if !CUDA
  let x = Tensor<Float>(1.0)
  let y = x.scalar! + 2.0

  let z = Tensor<Float>(y)
  let result = z + z
  expectEqual(6, result.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("test1RecvFloatScalar", test1RecvFloatScalar)

func test1RecvIntScalar() {
#if !CUDA
  let x = Tensor<Int32>(1)
  let y = x.scalar! + 2

  let z = Tensor<Int32>(y)
  let result = z + z
  expectEqual(6, result.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("test1RecvIntScalar", test1RecvIntScalar)

@inline(never)
func atariSimFloat(_ a: Tensor<Float>) -> Tensor<Float> {
  return a
}

func test1RecvFloatTensor() {
#if !CUDA
  let a = Tensor<Float>(1.0)
  // One send.
  print(a.toHost())
  // One recv.
  var b = atariSimFloat(a).toDevice()
  b += a
  print("final b = \(b.toHost())")
  expectEqual(2, b.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("test1RecvFloatTensor", test1RecvFloatTensor)

@inline(never)
func atariSimInt(_ a: Tensor<Int64>) -> Tensor<Int64> {
  return a
}

func test1RecvIntTensor() {
#if !CUDA
  let a = Tensor<Int64>(1)
  // One send.
  print(a.toHost())
  // One recv.
  var b = atariSimInt(a).toDevice()
  b += a
  print("final b = \(b.toHost())")
  expectEqual(2, b.scalar)
#endif //!CUDA
}
SendsRecvsTests.testCPU("test1RecvIntTensor", test1RecvIntTensor)

runAllTests()
