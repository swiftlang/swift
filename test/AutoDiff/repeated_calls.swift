// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var RepeatedCallsTests = TestSuite("RepeatedCalls")

RepeatedCallsTests.testWithLeakChecking("Repeat") {
  func mul2(_ x: Tracked<Float>) -> Tracked<Float> {
    return 2 * x
  }
  func mul4(_ x: Tracked<Float>) -> Tracked<Float> {
    return mul2(mul2(x))
  }
  expectEqual(4, gradient(at: 0, in: mul4))
}

runAllTests()
