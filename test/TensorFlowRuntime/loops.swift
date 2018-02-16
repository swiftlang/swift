// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Tensor API tests.

import TensorFlow
import TestUtils
import StdlibUnittest

var LoopsTests = TestSuite("Loops")

// TODO(b/73088003): Fix test crashes on GPU.
LoopsTests.testCPU("simpleCounterLoop") {
  // When using TF C API and GPU is available, the computation will be placed
  // on GPU automatically, and GPU support for loops is not yet ready.
  if !_RuntimeConfig.usesTFEagerAPI &&
    _ExecutionContext.global.gpuDeviceName != nil {
    return
  }

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
LoopsTests.testCPU("simpleCounterLoop_Int64") {
  // When using TF C API and GPU is available, the computation will be placed
  // on GPU automatically, and GPU support for loops is not yet ready.
  if !_RuntimeConfig.usesTFEagerAPI &&
    _ExecutionContext.global.gpuDeviceName != nil {
    return
  }

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
LoopsTests.testCPU("simpleCounterLoop_Int32") {
  // When using TF C API and GPU is available, the computation will be placed
  // on GPU automatically, and GPU support for loops is not yet ready.
  if !_RuntimeConfig.usesTFEagerAPI &&
    _ExecutionContext.global.gpuDeviceName != nil {
    return
  }

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
