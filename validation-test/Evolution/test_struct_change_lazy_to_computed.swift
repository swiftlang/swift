// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_change_lazy_to_computed


var ChangeLazyToComputedTest = TestSuite("ChangeLazyToComputed")

ChangeLazyToComputedTest.test("ChangeLazyToComputed") {
  do {
    var t = ChangeLazyToComputed(celsius: 0)
    expectEqual(t.fahrenheit, 32)
  }
}

runAllTests()
