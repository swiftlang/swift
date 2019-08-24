// RUN: %target-resilience-test
// REQUIRES: executable_test

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
