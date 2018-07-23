// RUN: %target-run-sese-loops-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize

import TensorFlow
import TensorFlowUnittest
import StdlibUnittest

var ControlFlowTests = TestSuite("ControlFlow")

func powerOfTwo(_ N: Int32) -> Tensor<Float>{
  var a = Tensor<Float>(1.0)
  for _ in 0..<N {
    a += a
  }
  return a
}

ControlFlowTests.testAllBackends("powerOfTwo") {
  expectNearlyEqualWithScalarTensor(2.0, powerOfTwo(1))
  expectNearlyEqualWithScalarTensor(4.0, powerOfTwo(2))
  expectNearlyEqualWithScalarTensor(8.0, powerOfTwo(3))
  expectNearlyEqualWithScalarTensor(1024.0, powerOfTwo(10))
}

func natSumWithBreak(_ breakIndex: Int32) -> Tensor<Int32> {
  var i: Int32 = 1
  var sum = Tensor<Int32>(0);
  let maxCount: Int32 = 100
  while i <= maxCount {
    sum += i
    if (i == breakIndex) {
      break;
    }
    i += 1
  }
  return sum;
}

ControlFlowTests.testAllBackends("natSumWithBreak") {
  expectEqualWithScalarTensor(3, natSumWithBreak(2))
  expectEqualWithScalarTensor(55, natSumWithBreak(10))
  expectEqualWithScalarTensor(5050, natSumWithBreak(-300))
  expectEqualWithScalarTensor(5050, natSumWithBreak(100))
  expectEqualWithScalarTensor(5050, natSumWithBreak(200))
}

runAllTests()
