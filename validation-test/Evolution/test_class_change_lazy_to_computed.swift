// RUN: %target-resilience-test
// REQUIRES: executable_test

// Use swift-version 4.
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

import StdlibUnittest
import class_change_lazy_to_computed


var ChangeLazyToComputedTest = TestSuite("ChangeLazyToComputed")

ChangeLazyToComputedTest.test("ChangeLazyToComputed") {
  do {
    let t = ChangeLazyToComputed(celsius: 0)
    expectEqual(t.fahrenheit, 32)
  }
}

runAllTests()
