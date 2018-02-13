// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import TestUtils
import StdlibUnittest

var LoopsTests = TestSuite("Loops")

LoopsTests.testCPUAndGPU("simpleCounterLoop") {
  let maxCount = 100
  var a = Tensor<Int32>(0)
  let b = Tensor<Int32>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(98, a.scalar)
}

// Explicitly use Int64 everywhere.
LoopsTests.testCPUAndGPU("simpleCounterLoop_Int64") {
  let maxCount = 100
  var a = Tensor<Int64>(0)
  let b = Tensor<Int64>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(98, a.scalar)
}

// Explicitly use Int32 everywhere.
LoopsTests.testCPUAndGPU("simpleCounterLoop_Int32") {
  let maxCount: Int32 = 100
  var a = Tensor<Int32>(0)
  let b = Tensor<Int32>(1)
  var count: Int32 = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(98, a.scalar)
}

runAllTests()
