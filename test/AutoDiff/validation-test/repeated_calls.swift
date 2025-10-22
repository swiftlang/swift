1// RUN: %target-run-simple-swift
// REQUIRES: executable_test

import StdlibUnittest
import DifferentiationUnittest

var RepeatedCallsTests = TestSuite("RepeatedCalls")

RepeatedCallsTests.test("Repeat") {
  func mul2(_ x: Float) -> Float {
    return 2 * x
  }
  func mul4(_ x: Float) -> Float {
    return mul2(mul2(x))
  }
  expectEqual(4, gradient(at: 0, of: mul4))
}

/* Temporary disabled until https://github.com/swiftlang/swift/issues/84840 is fixed
   We cannot use `Tracked<T>` :(
RepeatedCallsTests.testWithLeakChecking("Repeat-Tracked") {
  func mul2(_ x: Tracked<Float>) -> Tracked<Float> {
    return 2 * x
  }
  func mul4(_ x: Tracked<Float>) -> Tracked<Float> {
    return mul2(mul2(x))
  }
  expectEqual(4, gradient(at: 0, of: mul4))
}
*/

runAllTests()
