// RUN: %target-resilience-test
// REQUIRES: executable_test

import StdlibUnittest
import struct_change_stored_to_computed_static


var ChangeStoredToComputedTest = TestSuite("ChangeStoredToComputed")

ChangeStoredToComputedTest.test("ChangeStoredToComputed") {
  do {
    expectEqual(ChangeStoredToComputed.celsius, 0)
    expectEqual(ChangeStoredToComputed.fahrenheit, 32)
  }

  do {
    ChangeStoredToComputed.celsius = 10
    expectEqual(ChangeStoredToComputed.celsius, 10)
    expectEqual(ChangeStoredToComputed.fahrenheit, 50)
  }

  do {
    func increaseTemperature(_ t: inout Int) {
      t += 10
    }

    increaseTemperature(&ChangeStoredToComputed.celsius)

    expectEqual(ChangeStoredToComputed.celsius, 20)
    expectEqual(ChangeStoredToComputed.fahrenheit, 68)
  }
}

runAllTests()
