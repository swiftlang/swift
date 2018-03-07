// RUN: %target-run-simple-swift
// REQUIRES: executable_test
//
// Loop tests.

import TensorFlow
#if TPU
import TestUtilsTPU
#else
import TestUtils
#endif
import StdlibUnittest

var LoopsTests = TestSuite("Loops")

LoopsTests.testAllBackends("simpleCounterLoop") {
  guard shouldDoLoopTest() else { return }

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
LoopsTests.testAllBackends("simpleCounterLoop_Int64") {
  guard shouldDoLoopTest() else { return }

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
LoopsTests.testAllBackends("simpleCounterLoop_Int32") {
  guard shouldDoLoopTest() else { return }

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

// FIXME: Compiler bug (b/73607740)
// error: internal error generating TensorFlow graph:
// GraphGen cannot lower a 'send' to the host yet
#if false // Remove #if when fixed.
// This is derived from a TF Eager testcase.
TensorTests.testGPU("loopsAndConditions") {
  var a = Tensor<Int32>(6)
  var count = Tensor<Int32>(0)
  while (a != 1).scalar! {
    if (a % 2 == 0).scalar! {
      a = a / 2
    } else {
      a = 3 * a + 1
    }
    count += 1
  }
  expectEqual(8, count.scalar)
}
#endif

runAllTests()
