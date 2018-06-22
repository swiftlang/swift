// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

// Swift <-> TF sends/recvs tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var SendsRecvsTests = TestSuite("SendsRecvs")

@inline(never)
func test1Send() {
  var a = Tensor<Float>(1.0)
  // One send.
  printT(a.toHost())
  a += 2
  // This one should not be a send.
  printT(a.toHost())
  expectNearlyEqualWithScalarTensor(3, a)
}
SendsRecvsTests.testAllBackends("test1Send", test1Send)

@inline(never)
func test1SendWithParam() {
  func _test1SendWithParam(x: Float) {
    var a = Tensor<Float>(x)

    // On a TPU graph, to print out tensor `a` on host, we need to send it to TF
    // CPU via TPU outfeed. To supply outfeed with the tensor shape, we define
    // the intermediate `aSend`.
    let aSend = _scalarTensorWithShape(a)
    // One send.
    printT(aSend.toHost())

    a += 1
    // This one should not be a send.
    printT(a.toHost())
    expectNearlyEqualWithScalarTensor(2, a)
  }
  _test1SendWithParam(x: 1.0)
}
SendsRecvsTests.testAllBackends("test1SendWithParam", test1SendWithParam)

@inline(never)
func test2Sends() {
  var a = Tensor<Float>(1.0)
  // One send.
  printT(a.toHost())
  a += 2
  let aSend = _scalarTensorWithShape(a)
  // Another send.
  printT(aSend.toHost())
  a += 3
  // This one should not be a send.
  printT(a.toHost())
  expectNearlyEqualWithScalarTensor(6, a)
}
SendsRecvsTests.testAllBackends("test2Sends", test2Sends)

@inline(never)
func testSendsInALoop() {
  let maxCount = 3
  var count = 0
  var a = Tensor<Float>(0.0)
  let b = Tensor<Float>(1.0)
  while count < maxCount {
    a = _addScalarTensorsWithShape(a, b)
    let aSend = _scalarTensorWithShape(a)
    // One send.
    printT(aSend.toHost())
    count += 1
  }
  a += a
  // This is not a send.
  printT("final a = \(a.toHost())")
  expectNearlyEqualWithScalarTensor(3 * 2, a)
}
SendsRecvsTests.testAllBackends("testSendsInALoop", testSendsInALoop)

@inline(never)
func testSendsInALoopWithNoResultTensor() {
  let maxCount = 10
  var count = 1
  var a = Tensor<Float>(1.0)
  while count < maxCount {
    a += a
    // One send.
    print(a.toHost())
    count += 1
  }
}
SendsRecvsTests.testAllBackends("testSendsInALoopWithNoResultTensor",
                                testSendsInALoopWithNoResultTensor)

// TODO: Add TPU Support for the tests below.
func test1RecvFloatScalar() {
#if !TPU
  let x = Tensor<Float>(1.0)
  let y = x.scalar! + 2.0

  let z = Tensor<Float>(y)
  let result = z + z
  expectNearlyEqualWithScalarTensor(6, result)
#endif //!TPU  
}
SendsRecvsTests.testCPU("test1RecvFloatScalar", test1RecvFloatScalar)

func test1RecvIntScalar() {
#if !TPU
  let x = Tensor<Int32>(1)
  let y = x.scalar! + 2

  let z = Tensor<Int32>(y)
  let result = z + z
  expectEqualWithScalarTensor(6, result)
#endif //!TPU
}
SendsRecvsTests.testCPU("test1RecvIntScalar", test1RecvIntScalar)

@inline(never)
func atariSimFloat(_ a: Tensor<Float>) -> Tensor<Float> {
  return a
}

func test1RecvFloatTensor() {
#if !TPU
  let a = Tensor<Float>(1.0)
  // One send.
  printT(a.toHost())
  // One recv.
  var b = atariSimFloat(a).toAccelerator()
  b += a
  printT("final b = \(b.toHost())")
  expectEqualWithScalarTensor(2, b)
#endif //!TPU
}
SendsRecvsTests.testCPU("test1RecvFloatTensor", test1RecvFloatTensor)

@inline(never)
func atariSimInt(_ a: Tensor<Int64>) -> Tensor<Int64> {
  return a
}

func test1RecvIntTensor() {
#if !TPU
  let a = Tensor<Int64>(1)
  // One send.
  printT(a.toHost())
  // One recv.
  var b = atariSimInt(a).toAccelerator()
  b += a
  printT("final b = \(b.toHost())")
  expectEqualWithScalarTensor(2, b)
#endif //!TPU
}
SendsRecvsTests.testCPU("test1RecvIntTensor", test1RecvIntTensor)

runAllTests()
