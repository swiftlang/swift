// RUN: %target-run-simple-swift
// RUN: %target-run-simple-no-vjp-swift
// REQUIRES: executable_test

import StdlibUnittest

var RepeatedCallsTests = TestSuite("RepeatedCalls")

RepeatedCallsTests.test("Repeat") {
  func mul2(_ x: Float) -> Float {
    return 2 * x
  }
  func mul4(_ x: Float) -> Float {
    return mul2(mul2(x))
  }
  expectEqual(4, #gradient(mul4)(0))
}

runAllTests()
