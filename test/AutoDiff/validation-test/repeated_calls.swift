// RUN: %target-run-simple-swift(-Xfrontend -requirement-machine=off)
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
  expectEqual(4, gradient(at: 0, of: mul4))
}

runAllTests()
