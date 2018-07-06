// RUN: %target-run-simple-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Loop tests.

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var LoopsTests = TestSuite("Loops")

// Compute 1 << 10 == 1024
LoopsTests.testAllBackends("simpleCounterLoop_a") {
  let maxCount = 10
  // a cannot be an integer tensor due to a TensorFlow Eigen bug (b/77737504).
  var a = Tensor<Float>(1)
  var count = 1

  while count < maxCount {
    a += a
    count += 1
  }
  a += a
  expectNearlyEqual(1024.0, a.array.scalars[0])
}

LoopsTests.testAllBackends("simpleCounterLoop_ab") {
  let maxCount = 100
  var a = Tensor<Float>(0)
  let b = Tensor<Float>(1)
  var count = 0

  a -= b
  while count < maxCount {
    a += b
    count += 1
  }
  a -= b
  expectEqual(98, a.scalar)
}

LoopsTests.testAllBackends("SR8164") {
  func SR8164(count: Int32, expectedVal: Int32) {
    var a = Tensor<Int32>(count)
    let b = Tensor<Int32>(count)
    if (count == 100) {
      a += b
    } else {
      if (count <= 50) {
        a -= b
      } else {
        a += b
      }
    }
    a -= b
    expectEqualWithScalarTensor(expectedVal, a)
  }

  SR8164(count: 100, expectedVal: 100)
  SR8164(count: 30, expectedVal: -30)
  SR8164(count: 70, expectedVal: 70)
}

LoopsTests.testAllBackends("SR-8191") {
  let t = Tensor<Float>(1.0)
  var i = 0
  repeat {
    let y = t + t
    print(y)
    i += 1
  } while i < 10
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
