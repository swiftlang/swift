// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import class_add_override_inlinable


var ClassAddOverrideInlinableTest = TestSuite("ClassAddOverrideInlinable")

ClassAddOverrideInlinableTest.test("AddOverrideInlinable") {
  if getVersion() == 0 {
    expectEqual(42, getNumber())
  } else {
    expectEqual(69, getNumber())
  }
}

runAllTests()
