// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import class_resilient_add_virtual_method


var ClassAddVirtualMethodTest = TestSuite("ClassAddVirtualMethod")

ClassAddVirtualMethodTest.test("ClassAddVirtualMethod") {
  let c = AddVirtualMethod()

  do {
    expectEqual(1, c.firstMethod())
    expectEqual(2, c.secondMethod())
  }
}

runAllTests()
